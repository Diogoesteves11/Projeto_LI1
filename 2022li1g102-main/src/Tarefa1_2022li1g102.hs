{- |
Module      : Tarefa1_2022li1g102
Description : Validação de um mapa
Copyright   : André Sousa Miranda <a104088@alunos.uminho.pt>
              Diogo José Fernandes Esteves <a104004@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g102 where

import LI12223

{-|
A funçao validaObstaculo é uma função auxiliar da função principal "mapaVálido" que recebe um mapa e verifica se os obstáculos nele contidos estão de acordo com o tipo de terreno, returnando True caso os obstaculos correspondam ao terreno fornecido

>>> validaObstaculo (Mapa 3 [(Estrada 1, [Nenhum, Carro, Tronco])])
False

>>> validaObstaculo (Mapa 3 [(Estrada 1, [Nenhum, Carro, Nenhum])])
True
-}
validaObstaculo :: Mapa -> Bool 
validaObstaculo (Mapa l []) = True
validaObstaculo (Mapa l ((_,[]):ts)) = validaObstaculo (Mapa l ts)
validaObstaculo (Mapa l ((Rio x,(o:os)):ts)) | o == Nenhum = validaObstaculo (Mapa l ((Rio x,os):ts))
                                             | o /= Tronco = False 
                                             | otherwise = validaObstaculo (Mapa l ((Rio x,os):ts))
validaObstaculo (Mapa l ((Estrada x,(o:os)):ts)) | o == Nenhum = validaObstaculo (Mapa l ((Estrada x,os):ts))
                                                 | o /= Carro = False 
                                                 | otherwise = validaObstaculo (Mapa l ((Estrada x,os):ts))
validaObstaculo (Mapa l ((Relva,(o:os)):ts)) | o == Nenhum = validaObstaculo (Mapa l ((Relva,os):ts))
                                             | o /= Arvore = False 
                                             | otherwise = validaObstaculo (Mapa l ((Relva,os):ts))

{-|
A funçao validaRios é uma função auxiliar da função mapaVálidos e irá verificar se rios contiguos têm velocidades de sinal simétrico, indicando que os rios têm sentidos opostos. Caso esta condição se verifique, é returnado True

>>> validaRios (Mapa 3 [(Rio 1, [Nenhum, Carro, Tronco]),(Rio 1, [Nenhum,Nenhum, Nenhum])])
False

>>> validaRios (Mapa 3 [(Rio 1, [Nenhum, Carro, Tronco]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
True

-}


validaRios :: Mapa -> Bool
validaRios (Mapa l []) = True
validaRios (Mapa l ((Rio v,_):(Relva,_):ts)) = validaRios (Mapa l ts) 
validaRios (Mapa l ((Rio v,_):(Estrada v2,_):ts)) = validaRios (Mapa l ts)                                                    
validaRios (Mapa l ((_,[]):ts)) = validaRios (Mapa l ts)
validaRios (Mapa l ((Relva, o:os):ts)) = validaRios (Mapa l ts)
validaRios (Mapa l ((Estrada v, o:os):ts)) = validaRios (Mapa l ts)
validaRios (Mapa l ((Rio v,o:os):ts)) | velocidades (Mapa l ((Rio v,o:os):ts)) *  velocidades (Mapa l (tail ((Rio v,o:os):ts))) <= 0 = validaRios (Mapa l ts)
                                      | otherwise = False
                                        where 
                                              velocidades :: Mapa -> Int 
                                              velocidades (Mapa l []) = 0
                                              velocidades (Mapa l ((Rio v,o:os):ts)) = v
                                              velocidades (Mapa l ((Estrada v,o:os):ts)) = velocidades (Mapa l ts)
                                              velocidades (Mapa l ((Relva,o:os):ts)) = velocidades (Mapa l ts)

{-|
A função validaComprimentos irá verificar se Carros têm no máximo 3 unidades de comprimento e se Troncos têm no máximo 5. Es caso afirmatico, retorna True

>>> validaComprimentos (Mapa 4 [(Estrada 1, [Carro, Carro, Carro, Nenhum ]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
False

>>> validaComprimentos (Mapa 4 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum ]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
True
-}
validaComprimentos :: Mapa -> Bool
validaComprimentos (Mapa l []) = True
validaComprimentos (Mapa l ((Relva, (o:os)):ts)) = validaComprimentos (Mapa l ts)
validaComprimentos (Mapa l ((Rio v,(o:os)):ts)) | validaComprimentosAux1 (Mapa l ((Rio v,(o:os)): ts)) == True = validaComprimentos(Mapa l ts) | otherwise = False
validaComprimentos (Mapa l ((Estrada v,(o:os)):ts)) | validaComprimentosAux2 (Mapa l ((Estrada v,(o:os)): ts)) == True = validaComprimentos (Mapa l ts)
                                                |otherwise = False
{-|
Função Auxiliar da função validaComprimentos
-}
validaComprimentosAux1 :: Mapa -> Bool
validaComprimentosAux1 (Mapa l ((Rio v,[]):t)) = True
validaComprimentosAux1 (Mapa l ((Rio v,(o:os)): ts)) | length (o:os) <= 5 = True 
                                              | o == Tronco && head os == Tronco && head (tail os) == Tronco && head (tail(tail os)) == Tronco && head (tail(tail(tail os))) == Tronco && head (tail(tail(tail(tail os)))) == Tronco = False
                                              | otherwise = validaComprimentosAux1 (Mapa l ((Rio v,os):ts))
{-|
Função Auxiliar da função validaComprimentos
-}
validaComprimentosAux2 :: Mapa -> Bool 
validaComprimentosAux2 (Mapa l ((Estrada v,[]):ts)) = True
validaComprimentosAux2 (Mapa l ((Estrada v,(o:os)):ts))| length (o:os) <= 3 = True 
                                                       | o == Carro && head os == Carro && head(tail os) == Carro && head(tail(tail os))== Carro = False
                                                       | otherwise = validaComprimentosAux2 (Mapa l ((Estrada v,os):ts))


{-|
A função validaLinha verifica se, em cada lista de obstaculos, existe pelo menos um obstáculo "Nenhum", para que seja possivel ao jogador avançar no mapa. Em caso afirmatico retorna True

>>> validaLinha (Mapa 4 [(Estrada 1, [Carro, Carro, Carro, Carro ]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
False

>>>  validaLinha (Mapa 4 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
True
-}
validaLinha :: Mapa -> Bool
validaLinha (Mapa l ((_,[]):ts)) = True
validaLinha (Mapa l [(_,o:os)]) |elem Nenhum (o:os) = True
validaLinha (Mapa l ((_,o:os):ts)) | elem Nenhum (o:os) = validaLinha (Mapa l ts)
                                   | otherwise = False

{-|
A função validaLargura verifica se a largura fornecida corresponde ao comprimento da lista de obstáculos. Caso se confirme, retorna True

>>> validaLargura (Mapa 3 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
False

>>> validaLargura (Mapa 4 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum]),(Rio (-1), [Nenhum,Nenhum, Nenhum])])
True
-}
validaLargura :: Mapa -> Bool
validaLargura (Mapa l []) = True
validaLargura (Mapa l ((_,o:os):ts)) | l == length (o:os) = validaLargura (Mapa l ts)
                                     | otherwise = False

{-|
A função validaMapa verifica se não existem mais do que 5 estradas/ relvas consecutivas e se não existem mais do que 4 rios consecutivos

>>> validaMapa (Mapa 4 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum]),(Estrada 1, [Nenhum,Nenhum, Nenhum, Nenhum]),(Estrada 1, [Nenhum,Nenhum, Nenhum, Nenhum]),(Estrada 1, [Nenhum,Nenhum, Nenhum, Nenhum]),(Estrada 1, [Nenhum,Nenhum, Nenhum, Nenhum]),(Estrada 1, [Nenhum,Nenhum, Nenhum, Nenhum])])
False

>>> validaMapa (Mapa 4 [(Estrada 1, [Carro, Carro, Nenhum, Nenhum]),(Estrada 1, [Nenhum,Nenhum, Nenhum, Nenhum])])
True
-}
validaMapa :: Mapa -> Bool
validaMapa (Mapa largura []) = True
validaMapa (Mapa largura ((Relva, (x:y)): (Relva, (x1:y1)): (Relva, (x2:y2)): (Relva, (x3:y3)): (Relva, (x4:y4)): (Relva, (x5:y5)) :t)) = False
validaMapa (Mapa largura ((Estrada v ,(x:y)): (Estrada v1, (x1:y1)): (Estrada v2, (x2:y2)): (Estrada v3, (x3:y3)): (Estrada v4, (x4:y4)) :(Estrada v5, (x5:y5)):t)) = False
validaMapa (Mapa largura ((Rio v, (x:y)): (Rio v1, (x1:y1)):(Rio v2, (x2:y2)):(Rio v3, (x3:y3)): (Rio v4, (x4:y4)):t)) = False
validaMapa (Mapa largura ((p, (x:y)):t)) = validaMapa (Mapa largura t)



{-| 
A função principal mapaVálido recebe um Mapa e, servindo-se das funções auxiliares acima, retorna o valor da interseção dos seus valores lógicos, validando assim o mapa recebido.
-}
mapaVálido :: Mapa -> Bool
mapaVálido (Mapa l ((t,o:os):ts))
 | validaObstaculo (Mapa l ((t,o:os):ts)) && validaRios (Mapa l ((t,o:os):ts)) && validaComprimentos (Mapa l ((t,o:os):ts)) && validaLinha (Mapa l ((t,o:os):ts)) && validaLargura (Mapa l ((t,o:os):ts)) && validaMapa (Mapa l ((t,o:os):ts)) = True
 | otherwise = False


