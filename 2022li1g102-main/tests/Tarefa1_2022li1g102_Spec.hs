module Tarefa1_2022li1g102_Spec where

import LI12223
import Tarefa1_2022li1g102
import Test.HUnit

testsT1 = TestList [ 
        "Teste 1 (Mapa 3 [(Estrada 1, [Nenhum,Carro,Nenhum])]) " ~: True ~=? mapaVálido (Mapa 3 [(Estrada 1, [Nenhum,Carro,Nenhum])]),
        "Teste 2 (Mapa 3 [(Rio 1, [Nenhum,Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco])]) " ~: True ~=? mapaVálido (Mapa 3 [(Rio 1, [Nenhum,Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco])]),
        "Teste 3 (Mapa 3 [(Estrada 1, [Carro,Carro,Nenhum])]) " ~: True ~=? mapaVálido (Mapa 3 [(Estrada 1, [Carro,Carro,Nenhum])]),
        "Teste 4 (Mapa 4 [(Rio 1, [Tronco,Tronco,Tronco,Nenhum])]) " ~: True ~=? mapaVálido (Mapa 4 [(Rio 1, [Tronco,Tronco,Tronco,Nenhum])]),
        "Teste 5 (Mapa 3 [(Estrada 1, [Nenhum,Carro,Nenhum])]) " ~: True ~=? mapaVálido (Mapa 3 [(Estrada 1, [Nenhum,Carro,Nenhum])]),
        "Teste 6 (Mapa 3 [(Estrada 1, [Carro,Carro,Nenhum])]) " ~: True ~=? mapaVálido (Mapa 3 [(Estrada 1, [Carro,Carro,Nenhum])]),
        "Teste 7 (Mapa 3 [(Rio 1, [Nenhum,Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco])]) " ~: True ~=? mapaVálido (Mapa 3 [(Rio 1, [Nenhum,Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco]),(Rio 1,[Nenhum,Tronco,Tronco]),(Rio (-1),[Nenhum,Tronco,Tronco])])
        
 ] 

