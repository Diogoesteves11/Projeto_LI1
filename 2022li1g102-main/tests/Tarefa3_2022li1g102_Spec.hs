module Tarefa3_2022li1g102_Spec where

import LI12223
import Tarefa3_2022li1g102
import Test.HUnit

testsT3 = TestList [
     "Teste 1  (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima))" ~: Jogo (Jogador (1,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima),
     "Teste 2  (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Baixo))" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Baixo),
     "Teste 3  (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Baixo))" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Baixo),
     "Teste 4  (Jogo (Jogador (1,3)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima))" ~: Jogo (Jogador (1,3)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,3)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Cima),
     "Teste 5  (Jogo (Jogador (3,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Direita))" ~: Jogo (Jogador (3,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (3,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Direita),
     "Teste 6  (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Direita))" ~: Jogo (Jogador (3,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (2,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Direita),
     "Teste 7  (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Esquerda))" ~: Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Esquerda),
     "Teste 8  (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Esquerda))" ~: Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Move Esquerda),
     "Teste 9  (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado))" ~: Jogo (Jogador (2,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado),
     "Teste 10  (Jogo (Jogador (3,2)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado))" ~: Jogo (Jogador (3,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (3,2)) (Mapa 3 [(Rio 1, [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado),
     "Teste 11  (Jogo (Jogador (0,1)) (Mapa 3 [(Rio (-1), [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado))" ~: Jogo (Jogador (-1,1)) (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])]) ~=? animaJogo (Jogo (Jogador (0,1)) (Mapa 3 [(Rio (-1), [Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])])) (Parado)
 ]
