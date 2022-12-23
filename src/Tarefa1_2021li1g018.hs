{- |
Module      : Tarefa1_2021li1g018
Description : Validação de um potencial mapa
Copyright   : Hugo Arantes Dias <a100758@alunos.uminho.pt>;
            : Flávia Alexandra Silva Araújo <a96587@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g018 where

import LI12122

{- | A função 'validaPotencialMapa' serve para validar o mapa
dado pelo utilizador. Para isso, o mapa deve respeitar uma série
de parâmetros, que são:

1. Não existir mais que uma peça para uma dada coordenada.

2. Não existir mais que uma 'Porta' no mapa inteiro.

3. O bloco abaixo de uma 'Caixa' não pode ser um 'Vazio',
   ou seja a 'Caixa' não pode estar a flutuar

4. Tem de existir pelo menos um espaço vazio

5. O mapa tem de ter __Chão__ e o mesmo tem de ser contínúo,
   ou seja cada 'Bloco' pertencente ao chão tem de estar sempre
   ligado a outro 'Bloco'

-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = posicaoPeca pecas && umaPorta pecas && final3 pecas && vazio pecas && final5 pecas
                         

-- __PONTO 1__

{- | A função 'posicaoPeca' tem como objetivo determinar se existem ou
não peças com as mesmas coordenadas, condição essa essencial para que o
mapa seja considerado como válido. Para isso, de modo a facilitar o funcionamento
da função, é usada uma função auxiliar, denominada por 'posicaoPecaAux'.
-}

posicaoPeca :: [(Peca, Coordenadas)] -> Bool
posicaoPeca ((p,c):t) = posicaoPecaAux (soCoords ((p,c):t))


{- | A função 'posicaoPecaAux' é utilizada de modo a que, dada a lista
de coordenadas, verifica se existem ou não coordenadas com o mesmo valor.
Se existirem, a função retorna __False__, se não retorna __True__
-}

posicaoPecaAux ::  [Coordenadas] -> Bool 
posicaoPecaAux [] = True
posicaoPecaAux (h:t) | elem h t == True = False 
                   | otherwise = posicaoPecaAux t

{- | A função 'soCoords' é utilizada para separar a peça da coordenada
e assim dar uma lista apenas com as coordenadas sem as respetivas peças.
-}

soCoords :: [(Peca,Coordenadas)] -> [Coordenadas]
soCoords [] = []
soCoords ((p,c):t) = c : soCoords t

-- __PONTO 2__ 

{- | A função 'umaPorta' tem como objetivo verificar se o mapa dado
tem apenas uma só porta, e para isso usamos a uma função auxiliar
(de seu nome 'umaPortaAux').
-}

umaPorta :: [(Peca, Coordenadas)] -> Bool
umaPorta ((p,c):t) = umaPortaAux (soPeca ((p,c):t))

{- | A função 'umaPortaAux' percorre a lista e caso encontre uma peça 'Porta' 
então verifica se existe mais alguma no resto da lista, caso não exista
e esta porta seja única, retornará 'True', caso exista mais que uma, retornará
'False'. Retornará também 'False', caso não exista qualquer porta na lista dada.
-}
umaPortaAux :: [Peca] -> Bool 
umaPortaAux [] = True 
umaPortaAux (h:t) = if h == Porta && (elem Porta t == True) then False 
                  else if h == Porta && (elem Porta t == False) then True 
                  else if h /= Porta && (elem Porta t == True) then True
                  else False
                
{- | A função 'soPeca' é utilizada para separar a peça da coordenada
e assim dar uma lista apenas com as peças sem as respetivas coordenadas.
-}               
soPeca :: [(Peca,Coordenadas)] -> [Peca]
soPeca [] = []
soPeca ((p,c):t) = p : soPeca t

-- __PONTO 3__

{- | A função 'validaCaixa' tem então como objetivo confirmar que não existem
caixas flutuantes, ou seja tem de existir sempre um _'Bloco' ou uma 'Caixa' nas 
coordenadas abaixo da mesma. Utiliza uma função auxiliar 'confirmaCaixa'.
Nota: o ponto 3 foi reformulado devido à existência de falhas em testes por não 
termos considerado os casos em que a 'Caixa' tinha outra 'Caixa' embaixo (nesses
casos, o resultado deve ser 'True' e estava a retornar 'False').
-}

validaCaixa :: [(Peca, Coordenadas)] -> Bool
validaCaixa m = confirmaCaixa m []
   where confirmaCaixa [] _ = True
         confirmaCaixa (c@(Caixa, (x,y)):t) l = (any (\ x -> elem x t || elem x l) [(Bloco, (x,y+1)),(Caixa, (x,y+1))])  && confirmaCaixa t (c:l)
         confirmaCaixa (p:t) l = confirmaCaixa t (p:l)


-- __PONTO 4__

{- | A função 'vazio' percorre a lista e verifica se a mesma tem
pelo menos um espaço vazio.Caso isto se verifique então retornará 
o valor 'True', caso contrário o valor 'False'.
-}

vazio :: [(Peca, Coordenadas)] -> Bool 
vazio [] = False  
vazio m | length m < (x * y) = True 
        | otherwise = any (\ (p,(x,y)) -> p == Vazio) m 
            where (x,y) = dim m


{- | A função 'dim' é uma função auxiliar da 'vazio' que, dada a lista de 'Peca'
e as respetivas 'Coordenada', retorna o comprimento e a largura do mapa, isto é, o 
número de colunas e o número de linhas - valores estes que podem ser calculados através
da soma de 1 à maior abcissa e ordenada (que serão calculadas pelas funções auxiliares
'maiorX' e 'maiorY').
-}

dim:: [(Peca, Coordenadas)] -> Coordenadas  
dim p = ((maiorX p)+1,(maiorY p)+1)

{- | 'maiorX' é uma função auxiliar de 'dim' para calcular a maior abcissa.
-}
maiorX:: [(Peca, Coordenadas)] -> Int 
maiorX [(p,(x,y))] = x
maiorX ((p,(x,y)):t) = max x (maiorX t)

{- |'maiorY' é uma função auxiliar de 'dim' para calcular a maior ordenada.
-}
maiorY :: [(Peca,Coordenadas)]-> Int
maiorY [(p,(x,y))] = y
maiorY ((p,(x,y)):t) = max y (maiorY t)

-- __ PONTO 5__

{- | A função 'chaoValido' é a função principal para verificar se o _Chao_
é válido, isto é
-}
chaoValido :: [(Peca, Coordenadas)] -> Bool
chaoValido [] = False
chaoValido m = colunasValidas colunas
         where colunas = ordBlocos m

{- | A função 'ordBlocos' é uma das funções utilizadas em 'chaoValido'.
Esta função devolve listas com os _Bloco_ pertencentes às colunas
do mapa introduzido. Se a coluna não possuir elementos do tipo _Bloco_,
irá devolver lista vazia. 
-}
ordBlocos :: [(Peca,Coordenadas)] -> [[(Peca,Coordenadas)]]
ordBlocos m = ordBAux 0 soBlocos
         where maxColuna = maiorX m
               soBlocos = filter (\ (p,_) -> p == Bloco) m 
               ordBAux x l | x == maxColuna = [l]
                           | otherwise = cColunax : ordBAux (x+1) sColunax
                        where cColunax = filter (\ (p,(c,l)) ->  c == x) l
                              sColunax = filter (\ (p,(c,l)) ->  c /= x) l

{- | A função 'colunasValidas', tem a função de verificar se as colunas existentes no _Mapa_
fornecido são válidas, ou seja, se estão todas conectadas entre si e possuem pelo menos um _Bloco_.
Para tal, recorre à função auxiliar 'colunasAux'.
-}
colunasValidas :: [[(Peca,Coordenadas)]] -> Bool
colunasValidas [] = False
colunasValidas ([]:t) = False
colunasValidas (_:[]:t) = False
colunasValidas [x] = True 
colunasValidas (h1:h2:t) = if colunasAux h1 h2 then colunasValidas (h2:t)
                                               else False

{- | A função 'colunasAux' é a função auxiliar utilizada em 'colunasValidas', e
verifica se existe algum tipo de conexão entre Blocos em duas colunas consecutivas,
de forma a que o _Jogador_ consiga atravessar o _Mapa_, seja através de Blocos lado a lado ou
posicionados na diagonal, mas sempre de forma a que não existam espaços Vazios que tornem o 
_Mapa_ inválido.
-}
colunasAux :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Bool
colunasAux c1 c2 
          | elem y [b-1..b-1] = True
          | y > b = all (\n -> elem (Bloco,(x,n)) c1) [b+1..y-1]
          | otherwise = all (\n -> elem (Bloco,(a,n)) c2) [y+1..b-1]
    where (p1,(x, y)) = ultimo c1
          (p2,(a,b)) = ultimo c2


{- | 'ultimo' é uma função auxiliar utilizada em 'colunasAux', e tem a função
de retornar o último _Bloco_ da Coluna de Blocos introduzida.
-}
ultimo :: [(Peca,Coordenadas)] -> (Peca,Coordenadas)
ultimo ((p2,(x2,y2)):t) = (Bloco, (x2,y2))
        where l = maximum (map (\ (p1,(x1,y1)) -> y1) ((p2,(x2,y2)):t))
