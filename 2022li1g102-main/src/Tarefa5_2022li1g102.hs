{- |
Module      : Tarefa5_2022li1g102
Description : Deslize do mapa
Copyright   : André Sousa Miranda <a104088@alunos.uminho.pt>
              Diogo José Fernandes Esteves <a104004@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g102 where

import LI12223
import Tarefa2_2022li1g102

{-|A função deslizaJogo faz com que a última linha do mapa desapareça, ao mesmo tempo que uma nova linha no topo do mapa seja criada.
 
== Exemplos de utilização

>>> deslizaJogo 7 (Jogo (Jogador (1,2))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]), (Rio 1, [Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]))
Jogo (Jogador (1,3)) (Mapa 3 [(Estrada 2,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])])

>>> deslizaJogo 43 (Jogo (Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]), (Estrada 4, [Carro,Nenhum,Nenhum]),(Rio 2,[Nenhum,Nenhum,Tronco])]))
Jogo (Jogador (2,1)) (Mapa 3 [(Rio (-2),[Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 4,[Carro,Nenhum,Nenhum])])


-}

deslizaJogo :: Int  -- ^ inteiro aleatório relativa à  estendeMapa
                  -> Jogo -- ^ Argumento que indica um Jogo
                       -> Jogo -- ^ resultado
deslizaJogo n (Jogo (Jogador (x,y)) (Mapa z ((a,b):t))) = Jogo (Jogador (x,y+1)) (tira (estendeMapa (Mapa z ((a,b):t)) n))

{-|A função tira faz com que a última linha do mapa desapareça.
 
== Exemplos de utilização

>>> tira (Mapa 3 [(Rio 3,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 5,[Carro,Carro,Nenhum])])
Mapa 3 [(Rio 3,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore])]

>>> tira (Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 5,[Carro,Carro,Nenhum]),(Rio 1,[Tronco,Nenhum,Nenhum])])
Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 5,[Carro,Carro,Nenhum])]

-}
tira :: Mapa -- ^ Argumento que indica um Mapa
           -> Mapa -- ^ Resultado
tira (Mapa z ((a,b):t)) = Mapa z (init ((a,b):t))
