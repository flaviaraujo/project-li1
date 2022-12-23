{- |
Module      : Tarefa3_2021li1g018
Description : Representação textual do jogo
Copyright   : Hugo Arantes Dias <a100758@alunos.uminho.pt>;
            : Flávia Alexandra Silva Araújo <a96587@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g018 where

import LI12122
import Data.Char

instance Show Jogo where
  show = showFinal
{- | A tarefa 3 tem como objetivo mostrar por representação textual o Jogo
propriamente dito, onde:
*O bloco é representado por @X@
*A caixa é representada por @C@
*A porta é representada por @P@
*A personagem representa-se por @>@ ou @<@ dependendo da sua direção
*Espaços Vazios representam-se por " " (um espaço em branco)
__Exemplo:__
Assim com o input do mapa usado como exemplo na tarefa 2:
[[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
,[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco]
,[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
Espera-se a seguinte representação textual:
      X
      X
P   C<X
XXXXXXX
*Nota: sendo que optamos por colocar o jogador num dos espaços vazios.-}
showFinal :: Jogo -> String
showFinal (Jogo [[]] Jogador {}) = []
showFinal j@(Jogo _ (Jogador (x,y) d c ))
        | c == False = showJogo j  
        | c == True = showJogoAux (showJogo j) (temCaixa (x,y)) 


{- | A função 'showJogo' é uma das funções auxiliar de 'showFinal' e é responsável pela ordenação
dos elementos do @Mapa@ no seu formato caracteristicamente matricial, ordenando as @Pecas@ por @String@
de acordo com as suas @Coordenadas@. Esta também posiciona o @Jogador@.
-}
showJogo :: Jogo -> String
showJogo (Jogo [] (Jogador (x,y) _ _)) = ""
showJogo (Jogo ([]:t) (Jogador (x,y) direcao caixa)) = "\n" ++ showJogo (Jogo t (Jogador (x,y-1) direcao caixa))
showJogo (Jogo ((peca:xs):t) (Jogador (x,y) direcao caixa)) | x == 0 && y == 0 = (showJogador direcao) ++ showJogo (Jogo (xs:t) (Jogador (-1,-1) direcao caixa))
                                                            | y == 0           = showPeca peca ++ showJogo (Jogo (xs:t) (Jogador (x-1, y) direcao caixa))
                                                            | otherwise        = showPeca peca ++ showJogo (Jogo (xs:t) (Jogador (x,y) direcao caixa))


{- | A função 'showJogador' é usada como forma de determinar a direção do @Jogador@. 
Tendo em conta o referencial cartesiano com inspiração matricial utilizado no jogo,
consideramos @<@ como estando voltado para @Oeste@ e @>@ como estando voltado para @Este@.
-}

showJogador :: Direcao -> String
showJogador Este = ">"
showJogador Oeste = "<" 

{- | A função 'temCaixa' é utilizada para determinar as @Coordenadas@ do elemento @Caixa@, que, 
se estiver na posse do @Jogador@, encontra-se sempre acima do mesmo. Tendo em conta o referencial
cartesiano com inspiração matricial utilizado no jogo, consideramos essa localização como sendo
o mesmo que retirar 1 à ordenada do @Jogador@.
-}
temCaixa :: Coordenadas -> Coordenadas
temCaixa (x,y) = (x,y-1)

{- | A função 'showPeca' é usada para definir a representação textual de cada @Peca@.
-}

showPeca :: Peca -> String
showPeca Bloco = "X" 
showPeca Caixa = "C"
showPeca Porta = "P"
showPeca Vazio = " "          

{- | A função 'showJogoAux' é uma função auxiliar de 'showFinal' utilizada no caso do @Jogador@
ser portador de uma @Caixa@. Dadas as @Coordenadas@, auxilia ao seu posicionamento.
-}

showJogoAux :: String -> Coordenadas -> String
showJogoAux (h:t) (x,y) | y == 0 && x == 0 = 'C' : t
                        | y >= 1 && h == '\n' = h : showJogoAux t (x,y-1)
                        | y == 0 && x > 0 = h : showJogoAux t (x-1,y)
                        | otherwise = h : showJogoAux t (x,y)
