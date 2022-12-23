{- |
Module      : Tarefa4_2021li1g018
Description : Movimentação do personagem
Copyright   : Hugo Arantes Dias <a100758@alunos.uminho.pt>;
            : Flávia Alexandra Silva Araújo <a96587@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g018 where

import LI12122
import Tarefa3_2021li1g018

{- | A função 'correrMovimentos' é a função principal desta tarefa
sendo ela responsável por fazer acontecer os movimentos dados através
do input do utilizador. Nela estão presentes várias funções auxiliares
as quais serão explicadas posteriormente.
-}

correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo movimentos = aux jogo movimentos
        where aux :: Jogo -> [Movimento] -> Jogo
              aux jogo [] = jogo
              aux (Jogo m (Jogador (x,y) dir caixa)) (a:as) = correrMovimentos (moveJogador (Jogo m (Jogador (x,y) dir caixa)) a) as

{- | Esta função tem como objetivo fazer movimentar, realizar as ações
relativas à caixa, seja pegando nela ou largando-a (sendo que damos o
valor 'False' quando o jogador não possui uma 'Caixa', e o valor 'True'
quando o jogador possui uma 'Caixa'), e também a ação de 'trepaObjeto' seja um 'Bloco'
ou uma 'Caixa'
-}

moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo m (Jogador (x,y) dir caixa)) mov
            | mov == AndarEsquerda || mov == AndarDireita = avancar (Jogo m (Jogador (x,y) dir caixa)) mov
            | mov == Trepar = trepaObjeto (Jogo m (Jogador (x,y) dir caixa))
            | mov == InterageCaixa = moveCaixa (Jogo m (Jogador (x,y) dir caixa))

{- | A função 'coordPeca' serve para facilitar praticamente
todas as outras funções já que esta transmite a informação de qual
a peça numa 'Coordenada' específica.
-}

coordPeca :: Mapa -> Coordenadas -> (Int,Int) -> Peca
coordPeca [[x]] _ _ = x
coordPeca ([]:t) (x,y) (a,b) = coordPeca t (x,y) (0,b+1)
coordPeca ((r:res):t) (x,y) (a,b)
        | b == y && x == a = r
        | otherwise = coordPeca (res:t) (x,y) (a+1,b)

{- | A função 'avancar' corresponde à atualização do estado do 'Jogo' quando ocorre o 'Jogador' avança.
-}

avancar :: Jogo -> Movimento -> Jogo
avancar (Jogo m (Jogador (x,y) dir caixa)) mov
        | mov == AndarDireita && (coordPeca m (x+1,y) (0,0) == Bloco || coordPeca m (x+1,y) (0,0) == Caixa) && caixa == False = (Jogo m (Jogador (x,y) Este caixa))
        | mov == AndarEsquerda && (coordPeca m (x-1,y) (0,0) == Bloco || coordPeca m (x-1,y) (0,0) == Caixa) && caixa == False = (Jogo m (Jogador (x,y) Oeste caixa))
        | mov == AndarEsquerda = movimentar (Jogo m (Jogador (x,y) dir caixa)) AndarEsquerda (x,y) 
        | mov == AndarDireita && ((coordPeca m (x+1,y) (0,0) == Bloco || coordPeca m (x+1,y) (0,0) == Caixa) || coordPeca m (x+1,y-1) (0,0) == Bloco ) && caixa == True = (Jogo m (Jogador (x,y) Este caixa))
        | mov == AndarEsquerda && ((coordPeca m (x-1,y) (0,0) == Bloco || coordPeca m (x-1,y) (0,0) == Caixa) || coordPeca m (x-1,y-1) (0,0) == Bloco ) && caixa == True = (Jogo m (Jogador (x,y) Oeste caixa))
        | mov == AndarDireita = movimentar (Jogo m (Jogador (x,y) dir caixa)) AndarDireita (x,y)

{- | A função 'movimentar' corresponde à atualização do estado do 'Jogo' quando ocorre o 'Jogador' se movimenta.
-}
movimentar :: Jogo -> Movimento -> Coordenadas -> Jogo
movimentar (Jogo m (Jogador (x,y) dir caixa)) mov (a,b)
        | mov == AndarEsquerda && (coordPeca m (a-1,b+1) (0,0) == Bloco || coordPeca m (a-1,b+1) (0,0) == Caixa) = (Jogo m (Jogador (a-1,b) Oeste caixa))
        | mov == AndarDireita && (coordPeca m (a+1,b+1) (0,0) == Bloco || coordPeca m (a+1,b+1) (0,0) == Caixa) = (Jogo m (Jogador (a+1,b) Este caixa))
        | mov == AndarEsquerda = movimentar (Jogo m (Jogador (x,y) dir caixa)) mov (a,b+1)
        | mov == AndarDireita = movimentar (Jogo m (Jogador (x,y) dir caixa)) mov (a,b+1)         

{- | A função 'dropCaixa' tem como objetivo primordial largar a 'Caixa'
portada pelo jogador, nas diferentes direções possíveis, quer seja para a
Esquerda (Oeste) ou para a Direita (Este).
-}

dropCaixa :: Jogo -> Coordenadas -> Jogo
dropCaixa (Jogo m (Jogador (x,y) dir caixa)) (a,b)
    | dir == Oeste && (coordPeca m (a-1,b+1) (0,0)== Bloco || coordPeca m (a-1,b+1) (0,0) == Caixa) = (Jogo (removeVazio m (a-1,b) 0) (Jogador (x,y) Oeste False))
    | dir == Oeste = dropCaixa (Jogo m (Jogador (x,y) dir caixa)) (a,b+1)
    | dir == Este && (coordPeca m (a+1,b+1) (0,0)== Bloco || coordPeca m (a+1,b+1) (0,0) == Caixa) = (Jogo (removeVazio m (a+1,b) 0) (Jogador (x,y) Este False))
    | dir == Este = dropCaixa (Jogo m (Jogador (x,y) dir caixa)) (a,b+1)

{- | A função 'moveCaixa' serve para tal como diz o seu nome
para mover a caixa, e para tal, tomamos a opção de enumerar
todas as diferentes ocasiões onde tal ação pode acontecer.
-}

moveCaixa :: Jogo -> Jogo
moveCaixa (Jogo m (Jogador (x,y) dir caixa))
    | caixa == False && dir == Este && coordPeca m (x+1,y) (0,0) == Caixa && (coordPeca m (x+1,y-1) (0,0) == Bloco || coordPeca m (x+1,y-1) (0,0)==Caixa) = (Jogo m (Jogador (x,y) dir caixa))
    | caixa == False && dir == Oeste && coordPeca m (x-1,y) (0,0) == Caixa && (coordPeca m (x-1,y-1) (0,0) == Bloco || coordPeca m (x-1,y-1) (0,0)==Caixa) = (Jogo m (Jogador (x,y) dir caixa))
    | caixa == False && dir == Este && coordPeca m (x+1,y) (0,0) == Caixa =(Jogo (removeCaixa m (x+1,y) 0) (Jogador (x,y) dir True))
    | caixa == False && dir == Oeste && coordPeca m (x-1,y) (0,0) == Caixa =(Jogo (removeCaixa m (x-1,y) 0) (Jogador (x,y) dir True))
    | caixa == False && (dir == Oeste || dir == Este) = (Jogo m (Jogador (x,y) dir caixa))
    | caixa == True && dir == Este && (coordPeca m (x+1,y-1) (0,0) == Bloco || coordPeca m (x+1,y-1) (0,0) == Caixa) && (coordPeca m (x+1,y-2) (0,0)==Bloco || coordPeca m (x+1,y-2) (0,0)==Caixa) = (Jogo m (Jogador (x,y) dir caixa))
    | caixa == True && dir == Oeste && (coordPeca m (x-1,y-1) (0,0) == Bloco || coordPeca m (x-1,y-1) (0,0) == Caixa) && (coordPeca m (x-1,y-2) (0,0)==Bloco || coordPeca m (x-1,y-2) (0,0)==Caixa) = (Jogo m (Jogador (x,y) dir caixa))
    | caixa == True && dir == Este && (coordPeca m (x+1,y-1) (0,0) == Bloco || coordPeca m (x+1,y-1) (0,0) == Caixa) = (Jogo (removeVazio m (x+1,y-2) 0) (Jogador (x,y) dir False))
    | caixa == True && dir == Oeste && (coordPeca m (x-1,y-1) (0,0) == Bloco || coordPeca m (x-1,y-1) (0,0) == Caixa) = (Jogo (removeVazio m (x-1,y-2) 0) (Jogador (x,y) dir False))
    | caixa == True && (dir == Este || dir == Oeste) = dropCaixa (Jogo m (Jogador (x,y) dir caixa)) (x,y)




{- | Esta função aplica uma troca entre uma caixa e um vazio
substituindo, uma Peça 'Caixa' por um espaço vazio.
-}

trocaCaixaVazio :: [Peca] -> Int -> Int -> [Peca]
trocaCaixaVazio [] _ _ = []
trocaCaixaVazio (h:t) x a
    | x /= a = h : trocaCaixaVazio t x (a+1)
    | otherwise = Vazio : trocaCaixaVazio t x (a+1)

{- | 'removeVazio' é uma função que utiliza como auxiliar
a função anterior, e a sua utilidade é remover um espaço
vazio específico do mapa.
-}

removeVazio :: Mapa -> Coordenadas -> Int -> Mapa
removeVazio [] _ _ = []
removeVazio (h:t) (x,y) b
    | y /= b = h : removeVazio t (x,y) (b+1)
    | otherwise = trocaVazioCaixa h x 0 : removeVazio t (x,y) (b+1)

{- | Esta função aplica uma troca entre um espaço vazio e
uma caixa substituindo, um espaço vazio por uma Peça 'Caixa'.
-}

trocaVazioCaixa :: [Peca] -> Int -> Int -> [Peca]
trocaVazioCaixa [] _ _ = []
trocaVazioCaixa (h:t) x a
    | x == a = Caixa : t
    | otherwise = h : trocaVazioCaixa t x (a+1)

{- | Tal como a 'removeVazio', a 'removeCaixa' é uma função que tem
como utilidade fundamental remover uma peça 'Caixa' do mapa utilizando
como auxiliar a função anterior 'trocaVazioCaixa'.
-}

removeCaixa :: Mapa -> Coordenadas -> Int -> Mapa
removeCaixa [] _ _ = []
removeCaixa (h:t) (x,y) b
    | y /= b = h : removeCaixa t (x,y) (b+1)
    | otherwise = trocaCaixaVazio h x 0 : removeCaixa t (x,y) (b+1)

{- A função 'trepaObjeto' apresenta um papel fundamental no jogo, já
que é esta que permite ao jogador trepar Peças. Peças essas que podem
ser 'Caixas' ou 'Blocos'. Para tal enumeramos todos os diferentes
casos possíveis, sendo estes as possíveis direções do jogador e etc.
-}

trepaObjeto :: Jogo -> Jogo
trepaObjeto (Jogo m (Jogador (x,y) dir caixa))
        | dir == Este && (coordPeca m (x+1,y) (0,0) == Bloco || coordPeca m (x+1,y) (0,0) == Caixa) && (coordPeca m (x+1,y-1) (0,0) == Bloco || coordPeca m (x+1,y-1) (0,0) == Caixa) = (Jogo m (Jogador (x,y) dir caixa))
        | dir == Oeste && (coordPeca m (x-1,y) (0,0) == Bloco || coordPeca m (x-1,y) (0,0) == Caixa) && (coordPeca m (x-1,y-1) (0,0) == Bloco || coordPeca m (x-1,y-1) (0,0) == Caixa) = (Jogo m (Jogador (x,y) dir caixa))
        | dir == Oeste && caixa == True && (coordPeca m (x-1,y) (0,0) == Bloco || coordPeca m (x-1,y) (0,0) == Caixa) && coordPeca m (x-1,y-2) (0,0) == Bloco = (Jogo m (Jogador (x,y) dir caixa))
        | dir == Este && caixa == True && (coordPeca m (x+1,y) (0,0) == Bloco || coordPeca m (x+1,y) (0,0) == Caixa) && coordPeca m (x+1,y-2) (0,0) == Bloco = (Jogo m (Jogador (x,y) dir caixa))     
        | dir == Este && (coordPeca m (x+1,y) (0,0) == Bloco || coordPeca m (x+1,y) (0,0) == Caixa) = (Jogo m (Jogador (x+1,y-1) dir caixa))
        | dir == Oeste && (coordPeca m (x-1,y) (0,0) == Bloco || coordPeca m (x-1,y) (0,0) == Caixa) = (Jogo m (Jogador (x-1,y-1) dir caixa))  
        | dir == Oeste = (Jogo m (Jogador (x,y) dir caixa))
        | dir == Este = (Jogo m (Jogador (x,y) dir caixa))
