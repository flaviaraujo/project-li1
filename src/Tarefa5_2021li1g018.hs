{- |
Module      : Tarefa5_2021li1g018
Description : Aplicação Gráfica
Copyright   : Hugo Arantes Dias <a100758@alunos.uminho.pt>;
            : Flávia Alexandra Silva Araújo <a96587@alunos.uminho.pt>;


Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

{-- | NOTA: Para executar o jogo, executar os seguintes comandos:
   * ghc Tarefa5_2021g018
   * ./Tarefa5_2021g018

Para sair do executável, clicar na tecla Esc.
--}
module Main where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa3_2021li1g018
import Tarefa2_2021li1g018
import Tarefa4_2021li1g018
import LI12122

-- | Elementos presentes na representação gráfica do jogo no seu estado inicial.
type EstadoGloss = (Menu, Jogo, Textures, PicturePlayer)

{- | Elementos presentes na representação gráfica de qualquer _Peca_,
nomeadamente 'Bloco', 'Porta', 'Caixa' e 'Vazio'. São representados pelo
tipo de 'Peca', a imagem que a vai representar e as correções de tamanho necessárias.
-}
type Textures = [(Peca, (Picture, (Float,Float)))]

-- | Elementos presentes na representação gráfica do 'Jogador'.

type Player = ((Float,Float),Direcao)

-- | Lista de imagens que o 'Jogador' pode tomar como representação gráfica.
type PicturePlayer = [Picture]

-- | Menu do 'Jogo'.
data Menu = Controlador Opcoes
          | ModoJogo Jogo
          | Ganhar

-- | Opções que serão usadas no 'Menu'. 
data Opcoes = Jogar
            | Sair


-- | A função 'altura' corresponde, tal como o nome sugere, às dimensões definidas para a altura da representação gráfica do Jogo.
altura :: Float
altura = 300

-- | A função 'comprimento' corresponde, tal como o nome sugere, às dimensões definidas para o comprimento da representação gráfica do Jogo.
comprimento :: Float
comprimento = -350

{- | A função 'l' define o valor do lado de cada bloco no jogo. Irá ajudar a ordenar as 'Pecas' no 'Mapa' 
da esquerda para a direita, de forma a que não fiquem sobrepostas e tendo em conta o referencial cartesiano
utilizado neste projeto, cuja origem é no canto superior esquerdo.
-}
l :: Float
l = 116

-- | A função 'estadoInicial' carrega o estado inicial do jogo, utilizando a função 'lvl1' para o mapa.
estadoInicial :: Jogo
estadoInicial = Jogo lvl1 (Jogador (8,1) Oeste False)

-- | A função 'estadoGlossInicial' encarrega-se de produzir o estado gráfico inicial do jogo.
estadoGlossInicial :: Textures -> PicturePlayer -> EstadoGloss
estadoGlossInicial textures pp = (Controlador Jogar, estadoInicial, textures, pp)

{- | A função 'reageEventoGloss' tem o objetivo de alterar o estado gráfico do jogo tendo em conta alguma interação
realizada através da utilização das teclas de seta ou a tecla enter.
Utiliza a função 'moveJogador' da Tarefa 4, a função 'estadoInicial' e a função auxiliar 'ganhaJogo'.
-}
reageEventoGloss :: Event  -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (menu ,estadoInicial, textures, pp) = (menu, moveJogador estadoInicial Trepar,textures,pp)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (menu ,estadoInicial, textures, pp)  = (menu,moveJogador estadoInicial InterageCaixa, textures, pp)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (menu ,estadoInicial, textures, pp)  = (menu,moveJogador estadoInicial AndarDireita, textures, pp)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (menu ,estadoInicial, textures, pp)  = (menu,moveJogador estadoInicial AndarEsquerda, textures, pp)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Controlador Jogar,jogo,textures,pp) = (ModoJogo jogo, jogo ,textures,pp)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo (Jogo m (Jogador (x,y) dir c)), jogo,textures,pp) = (ModoJogo estadoInicial,jogo,textures,pp)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Ganhar,estadoInicial, textures, pp) = (Controlador Jogar,(Jogo lvl1 (Jogador (8,1) Oeste False)),textures,pp)
reageEventoGloss _ (ModoJogo (Jogo m (Jogador coords d c)), jogo,textures,pp) = if ganhaJogo jogo then (Ganhar,jogo,textures,pp) else (ModoJogo (Jogo m (Jogador coords d c)), jogo,textures,pp)
reageEventoGloss  _ e = e


{- | A função 'ganhaJogo' é uma função auxiliar de 'reageEventoGloss' cujo
objetivo é identificar se as 'Coordenadas' do 'Jogador' são as mesmas que as
da 'Porta' (utiliza as funções 'porta' e 'desconstroiMapa' (da Tarefa 2) para tal)
pois chegar à 'Porta' é o sinal indicativo que ganhou o jogo.
-}
ganhaJogo :: Jogo -> Bool
ganhaJogo (Jogo mapa (Jogador (x,y) d c)) = (x,y) == porta (desconstroiMapa mapa)

-- | A função auxiliar 'porta' determina as 'Coordenadas' do 'Mapa' onde se encontra a 'Porta'.
porta :: [(Peca,Coordenadas)] -> Coordenadas
porta ((p,(x,y)):t) = if p == Porta then (x,y) else porta t

{- | A função 'reageTempoGloss' altera o estado do jogo tendo em conta a passagem do tempo.
Neste jogo, não existem condições especiais para a passagem do tempo, pois o jogo não será
influenciado pela mesma, pelo que não foram adicionadas.
-}
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss _ e = e

-- | A função 'getMapa' define um _Mapa_ tendo em conta o 'Jogo' introduzido.
getMapa :: Jogo -> Mapa
getMapa (Jogo mapa (Jogador (x,y) dir c)) = mapa

-- | A função 'getPlayer' define o estado do 'Jogador' tendo em conta o 'Jogo' introduzido.
getPlayer :: Jogo -> Jogador
getPlayer (Jogo m (Jogador (x,y) dir c)) = Jogador (x,y) dir c 

-- | A função 'fps' define os frames.
fps :: Int
fps = 30

-- | A função 'dim' define a janela do executável do jogo.
dm :: Display
dm = InWindow "Block Dude" (1600, 970) (0, 0)

-- | A função 'vazio' define a representação gráfica das peças 'Vazio'.
vazio :: Picture
vazio = Blank


{- | A função 'desenhaPeca' encarrega-se de representar graficamente as 'Pecas'
utilizadas com as dimensões necessárias, redimensionando-as com a função 'Translate'.
-}
desenhaPeca :: Float -> Float -> Peca -> Textures -> Picture
desenhaPeca x y peca textures = Translate abss ordd texture
  where tuple   = (fromJust . lookup peca) textures
        texture = fst tuple
        abss   = ((+x) . fst . snd) tuple
        ordd   = ((+y) . snd . snd) tuple

{- | A função 'desenhaMapa' utiliza a função 'desenhaLinha' recursivamente para 
desenhar o 'Mapa' linha por linha.
-}
desenhaMapa :: Float -> Float -> Mapa -> Textures -> [Picture]
desenhaMapa _ _ [] _= []
desenhaMapa x y (h:t) textures = linha ++ resto
  where linha = desenhaLinha x y h textures
        resto = desenhaMapa x (y-l) t textures

{- | A função 'desenhaLinha' desenha a primeira linha do 'Mapa' (a cabeça da lista).
É uma função auxiliar de 'desenhaMapa'.
-}
desenhaLinha :: Float -> Float -> [Peca] -> Textures -> [Picture]
desenhaLinha _ _ [] _= []
desenhaLinha x y (h:t) textures = peca : resto
  where peca = desenhaPeca x y h textures
        resto = desenhaLinha (x+l) y t textures

{- | A função 'desenhaPlayer' encarrega-se da representação gráfica que o 'Jogador'
pode tomar, tendo em conta a sua direção e se carrega uma 'Caixa' ou não.
Utiliza a função 'Translate' para redimensionar e as funções auxiliares 'absPlayer'
e 'ordPlayer' para redimensionar o 'Jogador' tendo em conta as dimensões do 'Mapa'
(definidas em 'comprimento' e 'altura' e o lado de cada elemento no 'Mapa' - 'l'.)
-}
desenhaPlayer :: Jogador -> PicturePlayer -> Picture
desenhaPlayer (Jogador (x, y) d c) pp
   | d == Este && c == False = Translate (absPlayer x) (ordsPlayer y) (pp !! 0)
   | d == Oeste && c == False = Translate (absPlayer x) (ordsPlayer y) (pp !! 1)
   | d == Este && c == True = Translate (absPlayer x) ((ordsPlayer y)+58) (pp !! 2)
   | d == Oeste && c == True = Translate (absPlayer x) ((ordsPlayer y)+58) (pp !! 3)

{-| 'absPlayer' é uma função auxiliar de 'desenhaPlayer' cuja função é converter
o comprimento da imagem do 'Jogador' tendo em conta o comprimento do 'Mapa' (função
'comprimento') e a largura dos elementos no 'Mapa' (função 'l').
-}
absPlayer :: Int -> Float
absPlayer x = comprimento + realToFrac (x-1)*l + succ l

{-| 'ordPlayer' é uma função auxiliar de 'desenhaPlayer' cuja função é converter
a altura da imagem do 'Jogador' tendo em conta a altura do 'Mapa' (função 'altura')
e a largura dos elementos no 'Mapa' (função 'l').
-}
ordsPlayer :: Int -> Float
ordsPlayer y = altura - realToFrac (y)*l

{- | A função 'desenhaMenu' representa graficamente os diferentes estados do 'Menu',
seja o 'Menu' para iniciar a partida, ou o 'Menu' que anuncia a vitória.
Utiliza a função auxiliar 'desenhaTexto'.
É uma das funções auxiliares de 'desenhaEstadoGloss'.
-}
desenhaMenu :: EstadoGloss -> Picture
desenhaMenu (Controlador Jogar,jogo,textures,pp) = Pictures [Color black $ desenhaTexto "BLOCK DUDE", Color black $ Translate (50) (-70) $ desenhaTexto "JOGAR"]
desenhaMenu (Controlador Sair,jogo,textures,pp) = Pictures [Color black $ desenhaTexto "BLOCK DUDE", Color black $ Translate (50) (-70) $ desenhaTexto "JOGAR"]
desenhaMenu (ModoJogo (Jogo m (Jogador (x,y) dir c)), jogo,textures,pp) = Pictures pp
desenhaMenu (Ganhar, jogo,textures,pp) = Pictures [Color black $ Translate (-50) (-50) $ desenhaTexto "GANHOU!"]

{- | A função 'desenhaTexto' é uma função auxiliar de 'desenhaMenu', cuja função
é redimensionar o texto presente no 'Menu' usando 'Translate' e 'Scale'.
-}
desenhaTexto :: String -> Picture
desenhaTexto option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option


{- | A função 'desenhaEstadoGloss' é a função principal que, através das funções
'getMapa', 'getPlayer', 'desenhaMapa', 'desenhaPlayer' e 'desenhaMenu', desenha a 
reprsentação gráfica do estado atual do Jogo, utilizando-as para desenhar o 'Mapa',
o 'Jogador' e o 'Menu'.
-}
desenhaEstadoGloss :: EstadoGloss ->  Picture
desenhaEstadoGloss (ModoJogo jogo,estadoInicial, pecas, pp) = Pictures desenho
   where
        mapa           = getMapa estadoInicial
        jogador        = getPlayer estadoInicial
        desenhoMapa    = desenhaMapa comprimento altura mapa pecas
        desenhoPlayer  = desenhaPlayer jogador pp
        desenho        = desenhoPlayer : desenhoMapa
desenhaEstadoGloss (Controlador Jogar,jogo,pecas,pp) = desenhoMenu (Controlador Jogar,jogo,pecas,pp)
 where
        desenhoMenu = desenhaMenu
desenhaEstadoGloss (Controlador Sair,jogo,pecas,pp) = desenhoMenu (Controlador Jogar,jogo,pecas,pp)
 where
        desenhoMenu = desenhaMenu
desenhaEstadoGloss (Ganhar,jogo,pecas,pp) = Pictures [Color black $ Translate (-50) (-50) $ desenhaTexto "GANHOU!"]
 where
        desenhoMenu = desenhaMenu


{- | A função 'main' é a que permite a interação entre o jogador e o jogo,
através do IO. É a função principal, que carrega o jogo tendo em conta as 
características definidas nas funções utilizadas, bem como as imagens correspondentes
a cada elemento do jogo.
-}
main :: IO ()
main = do
  bloco <- loadBMP "bloco.bmp"
  caixa <- loadBMP "caixa.bmp"
  porta <- loadBMP "porta.bmp"
  playerE <- loadBMP "playerE.bmp"
  playerO <- loadBMP "playerO.bmp"
  playercE <- loadBMP "playercE.bmp"
  playercO <- loadBMP "playercO.bmp"
  play
    dm
    white
    fps
    (estadoGlossInicial
    [
      (Bloco,(bloco,(0,0))),
      (Caixa,(caixa,(0,0))),
      (Porta,(porta,(0,0))),
      (Vazio,(vazio,(0,0)))
    ]
     [
         playerE,
         playerO,
         playercE,
         playercO
     ])
    desenhaEstadoGloss
    reageEventoGloss
    reageTempoGloss

-- | A função 'lvl1' representa o mapa utilizado ainda na sua forma matricial.
lvl1 :: Mapa
lvl1 = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Porta, Vazio, Bloco, Vazio, Vazio, Bloco, Caixa, Vazio, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

{- | CONSIDERAÇÕES FINAIS:
Esta tarefa possui funções da playlist de vídeos "Guião Gloss" do CeSIUM, que
podem ser acedidos através do link: https://www.youtube.com/playlist?list=PLadvWyx_6w6XiJ95A4MqSfmIaRVbXWFGS
Links das imagens utilizadas:

 * player: https://www.pngwing.com/en/free-png-yserw
 * player 2 versão: https://www.pngwing.com/pt/free-png-zrzvl
 * bloco: https://www.kindpng.com/imgv/TmwoxJb_mario-brick-png-super-mario-bros-block-pixel/
 * caixa: https://www.pngwing.com/pt/free-png-zdgvt
 * porta: https://www.pngegg.com/en/png-zodfj
-}
