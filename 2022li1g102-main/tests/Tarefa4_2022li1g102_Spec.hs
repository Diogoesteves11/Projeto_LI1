module Tarefa4_2022li1g102_Spec where

import LI12223
import Tarefa4_2022li1g102
import Test.HUnit

testsT4 = TestList [
      "Teste 1 (Jogo (Jogador (0,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum])]))" ~: True ~=? jogoTerminou (Jogo (Jogador (0,1)) (Mapa 3 [(Relva, [Nenhum, Arvore, Nenhum])])),
      "Teste 2 (Jogo (Jogador (3,1)) (Mapa 3 [(Estrada 3,[Nenhum, Carro, Carro])]))" ~: False ~=? jogoTerminou (Jogo (Jogador (3,1)) (Mapa 3 [(Estrada 3,[Nenhum, Carro, Nenhum])])),
      "Teste 3 (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 3,[Nenhum, Tronco, Nenhum])]))" ~: False ~=? jogoTerminou (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 3,[Nenhum, Tronco, Nenhum])])),
      "Teste 4 (Jogo (Jogador (3,1)) (Mapa 3 [(Estrada 3,[Nenhum, Carro, Carro])]))" ~: True ~=? jogoTerminou (Jogo (Jogador (3,1)) (Mapa 3 [(Estrada 3,[Nenhum, Carro, Carro])])), 
      "Teste 5 (Carro [Carro,Carro,Nenhum,Carro,Nenhum])" ~: [1,2,4] ~=? (elemIndices Carro [Carro,Carro,Nenhum,Carro,Nenhum])
      ]
