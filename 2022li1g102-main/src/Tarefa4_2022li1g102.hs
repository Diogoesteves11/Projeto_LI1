{- |
Module      : Tarefa4_2022li1g102
Description : Determinar se o jogo terminou
Copyright   : André Sousa Miranda <a104088@alunos.uminho.pt>
              Diogo José Fernandes Esteves <a104004@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g102 where

import LI12223

{-| A função jogoTerminou indica se o jogador perdeu o jogo, onde True significa que sim, quando se encontra fora do mapa, na água ou "debaixo" de um carro.

== Exemplos de utilização:

>>> jogoTerminou (Jogo (Jogador (0,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum])]))
True
>>> jogoTerminou (Jogo (Jogador (1,1)) (Mapa 3 [(Estrada 3,[Nenhum, Carro, Nenhum])]))
False
-}

jogoTerminou :: Jogo   -- ^ Argumento que indica as coordenadas do jogador e o cenário onde se encontra
                  -> Bool   -- ^ Resultado

jogoTerminou (Jogo (Jogador (x,y)) (Mapa z u))
   |(fst(x,y) > z) && fst(x,y) < 0 = True
   |snd(x,y) > (length u) && fst(x,y) < 0 = True
   |(eRio (fst (u !! (y)) ) == True) && (( snd( u !! (y)) !! (x)) == Nenhum) = True
   |(eEstrada (fst (u !! (y)) ) == True) && (( snd( u !! (y)) !! (x)) == Carro) = True
   |otherwise = False


eRio :: Terreno -> Bool
eRio (Rio _) = True
eRio (_) = False

eEstrada :: Terreno -> Bool
eEstrada (Estrada _) = True
eEstrada (_) = False


{-
jogoTerminou (Jogo (Jogador (x,y)) (Mapa z ((Relva, n):t))) = if -300<x && x<300 && -300<y && y<300
         then False
         else True
jogoTerminou (Jogo (Jogador (x,y)) (Mapa z ((Estrada v, n):t))) = if elem x (elemIndices Carro n)
    then True
    else if -300<x && x<300 && -300<y && y<300
         then False
         else True
jogoTerminou (Jogo (Jogador (x,y)) (Mapa z ((Rio v, n):t))) = if elem x (elemIndices Nenhum n)
         then True
         else if -300<x && x<300 && -300<y && y<300
         then False
         else True

 A função elemIndices indica uma lista de posições onde estarão as causas para o jogador perder.

== Exemplos de utilização:

>>> elemIndices 1 [1,2,3,4,5]
[1]
>>> elemIndices 4 [2,7,4,8,2]
[]


elemIndices :: Eq a => a -- ^ Argumento que indica a variável que se pretende procurar
                        -> [a] -- ^ Argumento que uma lista de variáveis
                            -> [Int] -- ^ Resultado
elemIndices x l = aux x l 0
        where aux :: Eq a => a -> [a] -> Int -> [Int]
              aux x [] i = []
              aux x (h:t) i
                  |x == h = (i+1) : aux x t (i+1)
                  |otherwise = aux x t (i+1)
                  
        -}
