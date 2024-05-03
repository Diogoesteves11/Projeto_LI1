module Tarefa2_2022li1g102_Spec where

import LI12223
import Tarefa2_2022li1g102
import Test.HUnit

test1T2 :: Test
test1T2 = TestLabel "Teste 1 Tarefa 2" $ test ["(Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]), (Relva, [Nenhum,Nenhum,Arvore])]) 2" ~: (Mapa 3 [(Estrada (-3),[Nenhum,Nenhum,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore])]) ~=? estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]), (Relva, [Nenhum,Nenhum,Arvore])]) 2]

test2T2 :: Test
test2T2 = TestLabel "Teste 2 Tarefa 2" $ test ["(Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Arvore]), (Rio 3, [Tronco, Tronco, Nenhum, Nenhum]), (Estrada (-1), [Carro, Nenhum, Nenhum, Carro])]) 4" ~: (Mapa 4 [(Estrada (-1),[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Arvore]),(Rio 3,[Tronco,Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum,Carro])]) ~=? (estendeMapa (Mapa 4 [(Relva, [Arvore, Nenhum, Arvore, Arvore]), (Rio 3, [Tronco, Tronco, Nenhum, Nenhum]), (Estrada (-1), [Carro, Nenhum, Nenhum, Carro])]) 4)]

test3T2 :: Test
test3T2 = TestLabel "Teste 3 Tarefa 2" $ test ["Mapa 3 [(Relva,[Arvore, Nenhum,Arvore]), (Estrada 3, [Carro,Carro,Nenhum]), (Rio (-4), [Tronco,Nenhum,Nenhum])" ~: ([Estrada 0,Rio 0,Relva]) ~=? (proximosTerrenosValidos (Mapa 3 [(Relva,[Arvore, Nenhum,Arvore]), (Estrada 3, [Carro,Carro,Nenhum]), (Rio (-4), [Tronco,Nenhum,Nenhum])]))]

test4T2 :: Test
test4T2 = TestLabel "Teste 4 Tarefa 2" $ test ["3 (Rio 5, [Tronco,Tronco,Nenhum,Tronco,Tronco])" ~: ([Nenhum,Tronco]) ~=? (proximosObstaculosValidos 3 (Rio 5, [Tronco,Tronco,Nenhum,Tronco,Tronco]))]

test5T2 :: Test
test5T2 = TestLabel "Teste 5 Tarefa 2" $ test ["(Rio 4) (Rio 6) 5" ~: (Rio (-5)) ~=? speed (Rio 4) (Rio 6) 5]

test6T2 :: Test
test6T2 = TestLabel "Teste 6 Tarefa 2" $ test ["[Rio (-4),Relva,Estrada 5,Relva] 56" ~: (Estrada 5) ~=? escalaT [Rio (-4),Relva,Estrada 5,Relva] 56]

test7T2 :: Test
test7T2 = TestLabel "Teste 7 Tarefa 2" $ test ["[Tronco,Nenhum,Nenhum,Tronco] 43" ~: (Nenhum) ~=? escalaO [Tronco,Nenhum,Nenhum,Tronco] 43]

test8T2 :: Test
test8T2 = TestLabel "Teste 8 Tarefa 2" $ test ["4 (Rio 4, [Tronco,Tronco,Nenhum,Nenhum]) 7" ~: [Tronco,Tronco,Nenhum,Nenhum] ~=? conO 4 (Rio 4, [Tronco,Tronco,Nenhum,Nenhum]) 7]

tests = TestList [test1T2,
                  test2T2,
                  test3T2,
                  test4T2,
                  test5T2,
                  test6T2,
                  test7T2,
                  test8T2
                 ]

