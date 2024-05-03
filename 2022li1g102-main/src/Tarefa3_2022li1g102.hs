{- |
Module      : Tarefa3_2022li1g102
Description : Movimentação do personagem e obstáculos
Copyright   : André Sousa Miranda <a104088@alunos.uminho.pt>
              Diogo José Fernandes Esteves <a104004@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g102 where

import LI12223

{-|
Função auxiliar da função elemIndices' que retorna a posiçao do elemento desejado na lista recebida
-}
indices :: Eq a =>Int-> a -> [a]-> [Int]
indices p x [] = []
indices p x (h:t)| x == h = p: indices (p+1) x t
                 | otherwise = indices (p+1) x t

{-| 
A função elemIndices' retorna a lista com o numero da posição onde o elemento x se encontra na lista l
-}
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' = indices 0 

{-|
Função auxiliar da função moveObs 1ue, a partir de uma velocidade, reordena a lista de obstaculos
-}
aux :: Velocidade -> [Obstaculo] -> Int -> [Obstaculo]
aux v (o:os) x | v > 0 && x < v = aux v (last os:o:init os) (x+1)
               | v < 0 && x > v = aux v (os++[o]) (x-1)
               |otherwise = o:os 


{-|
Função auxiliar da função animaJogo, que movimenta os obstaculos de uma linha do mapa.
-}
moveObs :: [(Terreno , [Obstaculo])] -> [(Terreno,[Obstaculo])]
moveObs [] = []
moveObs ((Estrada v,o:os):ts) =  (Estrada v, aux v (o : os) 0) : moveObs ts
moveObs ((Rio v,o:os):ts) = (Rio v, aux v (o : os) 0) : moveObs ts
moveObs l = l
                         
{-|

A função moveJogador altera as coordenadas do jogador, consoante a jogada recebida e , ao mesmo tempo, reeordena os obstaculos de uma certa linha.

-}

moveJogador :: Jogo -> Jogada -> Jogo 
moveJogador (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) j|  y /= verificaRio (Mapa l ((t, o : os) : ts)) 0 = moveJogadorAux t1 j
                                                           | otherwise = moveEmTronco t1 j
                                                         where t1 = Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))
                                                                                                                 
{-|
A função moveJogadorAux move o jogador consoante a jogada fornecida
-}
moveJogadorAux :: Jogo -> Jogada -> Jogo 
moveJogadorAux (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) j| j == Move Cima && y >= 1 = Jogo (Jogador (x,y-1)) (Mapa l (moveObs [z] ++ moveObs ts))
                                                              | j == Move Baixo && y < (length m-1) = Jogo (Jogador (x,y+1)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                              | j == Move Direita && x < l = Jogo (Jogador (x+1,y)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                              | j == Move Esquerda && x >= 1 = Jogo (Jogador (x-1,y)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                              | otherwise = Jogo (Jogador (x,y)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                               where z = (t,o:os)
                                                                     m = (t,o:os):ts
                                                                     t1 = Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))


{-|
A função moveEmTronco move o Jogador caso este se encontre num rio, consoante os  criterios definidos
-}
moveEmTronco :: Jogo -> Jogada -> Jogo
moveEmTronco (Jogo (Jogador (x,y)) (Mapa l ((Rio v,o:os):ts))) j | elem x (elemIndices' Tronco (o:os)) && j == Move Direita = Jogo (Jogador (x+v+1,y)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                                 | elem x (elemIndices' Tronco (o:os)) && j == Move Esquerda = Jogo (Jogador (x+v-1,y)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                                 | elem x (elemIndices' Tronco (o:os)) && j == Parado = Jogo (Jogador (x+v,y)) (Mapa l (moveObs [z] ++ moveObs ts ))
                                                                 | otherwise = moveJogador t1 j 
                                                               where z = (Rio v,o:os)
                                                                     t1 = Jogo (Jogador (x,y)) (Mapa l ((Rio v,o:os):ts))
moveEmTronco (Jogo (Jogador (x,y)) (Mapa l (t:ts))) j = Jogo jogador' (Mapa l' (t:ll))
                                                 where (Jogo jogador' (Mapa l' ll)) = moveEmTronco (Jogo (Jogador (x,y)) (Mapa l ts)) j
                                                      



{-|
a função verificaLinha verifica se o jogador se encontra num rio
-}

verificaRio :: Mapa -> Int -> Int
verificaRio (Mapa l []) x = 0 
verificaRio (Mapa l ((Rio v,_):ts)) x = x 
verificaRio (Mapa l ((_,_):ts)) x = verificaRio (Mapa l ts) (x+1)



{-|
A função pósAtropelamento lida com o caso em que o atropelamento é confirmado (valor True na função atropelamento) e move os objetos da lonha consoante os critérios definidos
-}
posAtropelamento :: Jogo -> Jogo 
posAtropelamento (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,o:os):ts))) = Jogo (Jogador (x,y)) (Mapa l (moveObs x1))
                                                                where x1 = (Estrada 1,o:os):ts
posAtropelamento (Jogo (Jogador (x,y)) (Mapa l (t:ts))) = Jogo jogador' (Mapa l' ll)
                                                            where (Jogo jogador' (Mapa l' ll)) = posAtropelamento (Jogo (Jogador (x,y)) (Mapa l ts)) 

{-|
A função atropelamento compara a posição de um Carro com a posição do jogador e retorna o valor 'True' caso estes coincidam. Esta função apenas se aplica a terrenos do tipo "Estrada", uma vez que não faz sentido o jogador ser atropelado noutro tipo de terrenos
-} 

atropelamento :: Jogo -> Jogada -> Bool
atropelamento (Jogo (Jogador (x,y)) (Mapa l [])) j = False
atropelamento (Jogo (Jogador (x,y)) (Mapa l [(Estrada v,o:os)])) j= let pObs =  elemIndices' Nenhum (o:os)
                                                                       in not (elem x pObs)   
atropelamento (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,o:os):ts))) j|y /= 0 = atropelamento (Jogo (Jogador (x,y-1)) (Mapa l ts)) j
                                                                     |otherwise = let pObs =  elemIndices' Nenhum (o:os)
            in not (elem x pObs)    
atropelamento (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) j = atropelamento (Jogo (Jogador (x,y-1)) (Mapa l ts)) j
                                                                  

{-|
A função animaJogo movimenta os obstáculos (de acordo com a velocidade do terreno em que se encontram), e o personagem, de acordo com a jogada dada 
-}                      
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))) j
     | not (atropelamento z j) = moveJogador z j
     | otherwise = posAtropelamento z 
          where z = Jogo (Jogador (x,y)) (Mapa l ((t,o:os):ts))
                                                               

 
