module Tarefa5_2022li1g102_Spec where

import LI12223
import Tarefa5_2022li1g102
import Test.HUnit

testT5 = TestList [
         "Teste 1 (Jogo (Jogador (1,2))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]), (Rio 1, [Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]))" ~: Jogo (Jogador (1,3)) (Mapa 3 [(Estrada 3,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])]) ~=? deslizaJogo 15 (Jogo (Jogador (1,2))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]), (Rio 1, [Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])])),
         "Teste 2 (Jogo (Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]), (Estrada 4, [Carro,Nenhum,Nenhum]),(Rio 2,[Nenhum,Nenhum,Tronco])]))" ~: Jogo (Jogador (2,1)) (Mapa 3 [(Rio 5,[Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 4,[Carro,Nenhum,Nenhum])]) ~=? deslizaJogo 20 (Jogo (Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]), (Estrada 4, [Carro,Nenhum,Nenhum]),(Rio 2,[Nenhum,Nenhum,Tronco])])),
         "Teste 3 (Jogo (Jogador (1,2))(Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]), (Estrada 3, [Nenhum,Nenhum,Carro]),(Rio 1,[Nenhum,Nenhum,Tronco])]))" ~: Jogo (Jogador (1,3)) (Mapa 3 [(Estrada (-3),[Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Estrada 3,[Nenhum,Nenhum,Carro])]) ~=? deslizaJogo 2 (Jogo (Jogador (1,2))(Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]), (Estrada 3, [Nenhum,Nenhum,Carro]),(Rio 1,[Nenhum,Nenhum,Tronco])])),
         "Teste 4 (Jogo (Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]), (Rio 4, [Nenhum,Tronco,Tronco]),(Rio 2,[Nenhum,Nenhum,Tronco]),(Estrada 3,[Carro,Carro,Nenhum])]))" ~: Jogo (Jogador (2,1)) (Mapa 3 [(Rio (-2),[Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum]),(Rio 4,[Nenhum,Tronco,Tronco]),(Rio 2,[Nenhum,Nenhum,Tronco])]) ~=? deslizaJogo 43 (Jogo (Jogador (2,0))(Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]), (Rio 4, [Nenhum,Tronco,Tronco]),(Rio 2,[Nenhum,Nenhum,Tronco]),(Estrada 3,[Carro,Carro,Nenhum])]))
                   ]
