{- |
Module      : Tarefa2_2021li1g018
Description : Construção/Desconstrução do mapa
Copyright   : Hugo Arantes Dias <a100758@alunos.uminho.pt>;
            : Flávia Alexandra Silva Araújo <a96587@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g018 where

import LI12122

{- | A função 'constroiMapa' tem como objetivo, dada uma lista @l@ válida, retornar
uma grelha que represente um mapa devidamente ordenado, onde os espaços aos
quais não estão atribuidos quaisquer peças são preenchidos por um @Vazio@.
__Exemplo:__

@
constroiMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3))
             ,(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2))
             ,(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0))
             ,(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]
@

Retorna como output:

@
[[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
,[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco]
,[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
@

-}


constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa ((p,(x,y)):l) = constroiMapaC ((p,(x,y)):l) (constroiMapaB ((p,(x,y)):l))


{- | A função 'constroiMapaA' é uma das 3 funções auxiliares de 'constroiMapa', cujo 
objetivo encontra-se em verificar se as 'Coordenadas' @x@ e @y@ existem no input dado 
pelo utilizador, e devolve a lista de 'Peca' correspondentes às mesmas. Se elas não 
existirem no input, a função vai introduzir um 'Vazio'. Recorre à função 'procuraPeca'
para descobrir a 'Peca' correspondente a uma 'Coordenada'.
-}

constroiMapaA :: [Coordenadas] -> [(Peca,Coordenadas)] -> [Peca]
constroiMapaA ((x1,y1):t) ((p,(x,y)):l)
    | elem (x1,y1) (snd (unzip(((p,(x,y)):l)))) = procuraPeca (x1,y1) ((p,(x,y)):l) : constroiMapaA t ((p,(x,y)):l)
    | otherwise = Vazio : constroiMapaA t ((p,(x,y)):l)
constroiMapaA _ _ = [] 

{- | A função 'constroiMapaB' é uma das 3 funções auxiliares de 'constroiMapa', cujo
objetivo encontra-se em ordenar as 'Coordenadas' pela ordenada, recorrendo à função 'contaOrd', para
poder aplicar a função 'constroiMapaA' nas mesmas, e obter uma lista de 'Peca' ordenada.
-}

constroiMapaB :: [(Peca,Coordenadas)] -> [Peca]
constroiMapaB [] = []
constroiMapaB ((p,(x,y)):l) = constroiMapaA (contaOrd (fim (snd(unzip(((p,(x,y)):l)))))) ((p,(x,y)):l)


{- | A função 'constroiMapaC' é uma das 3 funções auxiliares de 'constroiMapa', cujo
objetivo encontra-se em ordenar os elementos 'Peca' numa lista de lista de 'Peca' - um 'Mapa'.
-}

constroiMapaC :: [(Peca,Coordenadas)] -> [Peca] -> Mapa
constroiMapaC _ [] = []
constroiMapaC ((p,(x,y)):l) (h:hs) = take (maxX(snd(unzip(((p,(x,y)):l))))+1) (h:hs) : constroiMapaC ((p,(x,y)):l) (drop(maxX(snd(unzip(((p,(x,y)):l))))+1) (h:hs))


{- | A função 'procuraPeca' é uma função auxiliar de 'constroiMapaA'.
@procuraPeca (x1,y1) ((p,(x,y)):l)@ retorna a peca correspondente a uma
coordenada @(x1,y1)@.
-}
procuraPeca :: Coordenadas -> [(Peca,Coordenadas)] -> Peca
procuraPeca (x1,y1) ((p,(x,y)):l)
    | x1 == x && y1 == y = p
    | otherwise = procuraPeca (x1,y1) l   
                                                           

{- | Função 'fim', em que dada uma lista das 'Coordenadas',retirar a maior abcissa e
a maior ordenada de modo a definir assim o fim da lista.
Utiliza 3 funções auxiliares:

*soX para, dado uma lista de 'Coordenadas', devolver uma lista de abcissas 'Int';

*soY para, dado uma lista de 'Coordenadas', devolver uma lista de ordenadas 'Int';

*função pré-definida 'maximum', para achar o valor máximo.
-}

fim :: [Coordenadas] -> Coordenadas 
fim [] = (0,0)
fim ((x,y):l) = (maximum (soX ((x,y):l)) , maximum (soY ((x,y):l)))


{- | Funções 'soX' e 'soY' usadas para, dado uma lista de 'Coordenadas', obter listas do 
tipo 'Int' unicamente de abcissas e ordenadas, respetivamente.
-}

soX :: [Coordenadas] -> [Int]
soX [] = []
soX ((x,y):t) = x : soX t

soY :: [Coordenadas] -> [Int] 
soY [] = []
soY ((x,y):t) = y : soY t


{- | Função 'maxX' usada para, dado uma lista de 'Coordenadas', retornar a maior abcissa.

*Nota: Não é absolutamente necessária, porém ajuda a tornar a função "constroiMapaC" mais compacta.
-}

maxX :: [Coordenadas] -> Int
maxX l = maximum (soX l)

{- | A função 'contaOrd' é usada como função auxiliar da 'constroiMapaB'. Utiliza as funções 
'contaPecasX' e 'contaPecasY' para ordenar as abcissas @x@ e ordenadas @y@ numa lista de 'Coordenadas'.
-}

contaOrd :: Coordenadas -> [Coordenadas]
contaOrd (x,y) = reverse (contaPecasY (x,y))

{- | Função auxiliar para 'contaPecasY'
-}
contaPecasX :: Coordenadas -> [Coordenadas]
contaPecasX (x,y)
    | x < 0 = []
    | otherwise = (x,y) : contaPecasX (x-1,y)

{- | Função auxiliar para 'contaOrd'
-}
contaPecasY :: Coordenadas -> [Coordenadas]
contaPecasY (x,y)
    | y < 0 = []
    | otherwise = contaPecasX (x,y) ++ contaPecasY (x,y-1)


{- | A função 'desconstroiMapa' tem como objetivo fazer o contrário da função
'constroiMapa'. Assim, dado uma grelha que representa o @Mapa@, espera-se que
a função retorne uma lista com as @Pecas@ e as respetivas @Coordenadas@, retirando
os espaços denominados como @Vazios@.

__Exemplo:__

@
desconstroiMapa [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                ,[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
                ,[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco]
                ,[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
@

__Retorna o output:__

@
[(Bloco, (6,0)), (Bloco, (6,1)), (Porta, (0,2)), (Caixa, (4,2))
,(Bloco, (6,2)), (Bloco, (0,3)), (Bloco, (1,3)), (Bloco, (2,3))
,(Bloco, (3,3)), (Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,3))]
@

-}


desconstroiMapa :: Mapa -> [(Peca,Coordenadas)]
desconstroiMapa [[]] = []
desconstroiMapa ((h:hs):l) = desconstroiMapaA ((h:hs):l) 0 0


{- | Para o funcionamento da função 'desconstroiMapa', utilizamos a função auxiliar
'desconstroiMapaA' responsável por colocar as coordenadas certas nas respetivas peças 
através de 2 contadores: @x@ para as abcissas e @y@ para as ordenadas.

-}

desconstroiMapaA :: Mapa -> Int -> Int -> [(Peca,Coordenadas)]
desconstroiMapaA [[]] _ _ = []
desconstroiMapaA ([]:t) x y = removeVazio (desconstroiMapaA t 0 (y+1))
desconstroiMapaA ((h:hs):l) x y = removeVazio ((h,(x,y)) : (desconstroiMapaA (hs:l) (x+1) y))

{- | A função 'removeVazio' serve para remover as peças denominadas por @Vazio@, e funciona
através da comparação entre a @Peça@ e a string @Vazio@. Quando a função deteta
que a peça é igual à string anteriormente dita, esta ignora esse par,
passando assim para o próximo par por recursividade.

-}

removeVazio :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
removeVazio [] = []
removeVazio ((Vazio,c):t) = removeVazio t
removeVazio ((p,c):t) = (p,c) : removeVazio t

