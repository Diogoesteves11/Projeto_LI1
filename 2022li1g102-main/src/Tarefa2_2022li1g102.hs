{- |
Module      : Tarefa2_2022li1g102
Description : Geração contínua de um mapa
Copyright   : André Sousa Miranda <a104088@alunos.uminho.pt>
              Diogo José Fernandes Esteves <a104004@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g102 where

import LI12223
{-| A função estendeMapa gera e adiciona uma nova linha válida ao topo de um dado mapa.

== Exemplos de utilização:

>>> estendeMapa (Mapa 3 [(Estrada 2, [Carro,Nenhum,Carro]), (Rio 3, [Nenhum,Nenhum,Tronco])]) 1
Mapa 3 [(Estrada (-4),[Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro]),(Rio 3,[Nenhum,Nenhum,Tronco])]

>>> estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]), (Relva, [Nenhum,Nenhum,Arvore])]) 2
Mapa 3 [(Estrada (-3),[Nenhum,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore])]

-}
estendeMapa :: Mapa  -- ^ Argumento que indica um mapa
                 -> Int  -- ^ Argumento que indica o tipo de linha a adicionar
                      -> Mapa -- ^ Resultado
estendeMapa (Mapa n ((t,obs):xs)) o = Mapa n ((pT,pO):((t,obs):xs))
                        where pT = escalaT (proximosTerrenosValidos (Mapa n ((t,obs):xs))) o
                              pO = conO n (pT, []) o

{-| A função proximosTerrenosValidos gera a lista de terrenos possíveis de serem usados numa nova linha no topo do mapa dado.


                              
== Exemplos de utilização:

>>> proximosTerrenosValidos (Mapa 3 [(Rio 1, [Tronco,Nenhum,Nenhum]),(Rio (-2), [Tronco,Tronco,Nenhum]),(Rio 3, [Nenhum,Tronco,Nenhum]),(Rio (-4), [Nenhum,Tronco,Tronco])])
[Estrada 0,Relva]

>>> proximosTerrenosValidos (Mapa 3 [(Relva, [Arvore,Nenhum,Nenhum]),(Relva, [Arvore,Arvore,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum]),(Relva, [Nenhum,Nenhum,Arvore])])
[Estrada 0,Rio 0,Relva]
-}
proximosTerrenosValidos :: Mapa -- ^ Argumento que indica um mapa
                             -> [Terreno] -- ^ Resultado
proximosTerrenosValidos (Mapa n ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 1, Relva]
proximosTerrenosValidos (Mapa n ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 2, Relva]
proximosTerrenosValidos (Mapa n ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = [Rio 1, Estrada (-2)]
proximosTerrenosValidos m = [Estrada (-1), Rio (-2), Relva]

{-| A função validaLinha verifica se em qualquer linha existe, no mínimo, um “obstáculo” Nenhum.

== Exemplos de utilização:

>>> validaLinha [Tronco, Nenhum, Tronco]
True

>>> validaLinha [Carro, Carro, Carro]
False
-}
validaLinha :: [Obstaculo] -- ^ Argumento que indica uma lista de obstáculos
                        -> Bool -- ^ Resultado
validaLinha l = elem Nenhum l

{-| A função validaObstaculo verifica se numa lista de obstáculos não existem obstãculos em terrenos impróprios.

== Exemplos de utilização:

>>> validaObstaculo (Rio 7,[Tronco,Nenhum,Tronco])
True

>>> validaObstaculo (Rio 6,[Tronco,Carro,Tronco])
False

-}
validaObstaculo :: (Terreno,[Obstaculo]) -- ^ Argumento que indica uma linha de cenário
                           -> Bool -- ^ Resultado
validaObstaculo (Relva,o) | elem Tronco o || elem Carro o = False
                            | otherwise = True
validaObstaculo (Estrada l, o) | elem Tronco o || elem Arvore o = False
                                 | otherwise = True
validaObstaculo (Rio l, o) | elem Carro o || elem Arvore o = False
                             | otherwise = True

{-| A função proximosObstaculosValidos gerar a lista de obstáculos possíveis de serem usados para continuar uma dada linha do mapa. 

== Exemplos de utilização:

>>> proximosObstaculosValidos 5 (Rio 4, [Nenhum, Tronco, Nenhum, Tronco, Tronco])
[]

>>> proximosObstaculosValidos 5 (Estrada 4, [Nenhum,Carro,Nenhum,Carro])
[Nenhum,Carro]

-}
proximosObstaculosValidos :: Int -- ^ Argumento que indica o número de obstáculos
                               -> (Terreno, [Obstaculo]) -- ^ Argumento que indica a linha
                                                 -> [Obstaculo] -- ^ Resultado

proximosObstaculosValidos n (Estrada _, Carro:Carro:t) = [Nenhum]
proximosObstaculosValidos n (Rio _, Tronco:Tronco:Tronco:Tronco:xs) = [Nenhum]
proximosObstaculosValidos n (Relva, Arvore:Arvore:xs) = [Nenhum]
proximosObstaculosValidos n (Estrada _, Nenhum:Nenhum:t) = [Carro]
proximosObstaculosValidos n (Rio _, Nenhum:Nenhum:xs) = [Tronco]

proximosObstaculosValidos n (x,y)
       | notElem Nenhum y && (n == (-1) + length y) = [Nenhum]
       | otherwise = provl n (x,y)

provl :: Int -> (Terreno,[Obstaculo]) -> [Obstaculo]
provl n (Rio _, y) = [Tronco,Nenhum]
provl n (Estrada _, y) = [Carro,Nenhum]
provl n (Relva, y) = [Nenhum, Arvore]


{-| A função escalaT permite a geração de uma nova linha de terreno a partir de um inteiro aleatório (no intervalo [0, 100]) que ser usado para acrescentar alguma pseudo-aleatoriedade.

== Exemplos de utilização:

>>> escalaT [Rio 2, Relva, Estrada 5] 17
Relva

>>> escalaT [Rio 2, Relva, Estrada 5] 6
Rio 2
-}
escalaT :: [Terreno] -- ^ Argumento que indica uma lista de terrenos
                 -> Int -- ^ Argumento de pseudo-aleatoriedade
                      -> Terreno -- ^ Resultado
escalaT x y = x !! round(fromIntegral((y * length x + 5 - length x) :: Int) /100)

{-| A função escalaO permite a mesma coisa que a função escalaT, só que para uma lista de obstáculos.

== Exemplos de utilização:

>>> escalaO [Tronco, Nenhum, Tronco, Tronco] 24
Nenhum

>>> escalaO [Carro, Nenhum, Nenhum, Carro] 79
Carro
-}
escalaO :: [Obstaculo] -- ^ Argumento que indica uma lista de obstáculos
                   -> Int -- ^ Argumento de pseudo-aleatoriedade
                         -> Obstaculo -- ^ Resultado
escalaO x y = x !! round(fromIntegral((y * length x + 5 - length x) :: Int) /100)

{-| A função conO permite a obtenção de uma lista de obstáculos válida para a nova linha gerada.

== Exemplos de utilização:

>>> conO 3 (Estrada 5, [Carro, Nenhum, Carro]) 5
[Carro,Nenhum,Carro]

>>> conO 5 (Rio 1, [Nenhum, Tronco, Tronco]) 5
[Nenhum,Tronco,Tronco,Nenhum,Nenhum]
-}
conO :: Int -- ^ Argumento que indica o comprimento da lista de obstáculos
          -> (Terreno,[Obstaculo]) -- ^ Argumento que indica uma linha
                           -> Int -- ^ Argumento de pseudo-aleatoriedade
                               -> [Obstaculo] -- ^ Resultado
conO a (x,y) b
    |a == length y = y
    |otherwise = conO a (x,y ++ [escalaO (proximosObstaculosValidos a (x,y)) b]) b








