{- |
Module      : Main
Description : Criação do jogo
Copyright   : André Sousa Miranda <a104088@alunos.uminho.pt>
              Diogo José Fernandes Esteves <a104004@alunos.uminho.pt>

Módulo para a realização da Main do projeto de LI1 em 2022/23.
-}
module Main where

import LI12223
import Tarefa1_2022li1g102
import Tarefa2_2022li1g102
import Tarefa3_2022li1g102
import Tarefa4_2022li1g102
import Tarefa5_2022li1g102
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Data.Display
import System.Random

type EstadoGloss = (Menu,Estado,Imagens)

type Estado = (Jogo,Jogada,Points,Player) 

type Points = Int

data Imagens = 
       Imagens {rio           :: Picture,
                relva         :: Picture,
                estrada       :: Picture,
                carroDireito  :: Picture,
                carroEsquerda :: Picture,
                arvore        :: Picture,
                tronco        :: Picture,
                bee           :: Picture,
                frogback      :: Picture,
                título        :: Picture,
                menu          :: Picture,
                gameover      :: Picture,
                pausa         :: Picture
                }   



data Menu = Opcoes Opcao 
          | ModoJogo
          | ModoPerder
          | Pausa
          | Escolhe Player

data Opcao = Jogar
           | Sair

data Player = Sapo
           | Abelha

{-|
Funções que definem a altura, o comprimento e o lado do jogo.
-}
altura :: Float 
altura = 300      

comprimento :: Float
comprimento = -300

lado :: Float
lado = 100

{-|
Função que define o ínicio do jogo quando o jogador começar a jogar, de acordo com o Estado definido.
-}
estadoInicial :: Estado
estadoInicial = (Jogo (Jogador (4,5)) mapa1, Parado,0, Sapo)

mapa1 =
       Mapa 7 [(Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
               (Relva    ,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore])]

{-|
Função que permite o jogo carregar as imagens.
-}
carregarImagens :: IO Imagens
carregarImagens = do
    rio           <- loadBMP "river.bmp"
    relva         <- loadBMP "grass.bmp"
    tronco        <- loadBMP "log.bmp"
    bee           <- loadBMP "bee.bmp"
    frogback      <- loadBMP "frog-back.bmp"
    arvore        <- loadBMP "tree.bmp"
    estrada       <- loadBMP "road.bmp"
    carroDireito  <- loadBMP "car-right.bmp"
    carroEsquerda <- loadBMP "car-left.bmp"
    título        <- loadBMP "título2.bmp"
    menu          <- loadBMP "fundo_1.bmp"
    gameover      <- loadBMP "gameover.bmp"
    pausa         <- loadBMP "pausa.bmp"
    
    return Imagens { bee           = bee,
                     frogback      = frogback,
                     rio           = rio,
                     relva         = relva,
                     tronco        = tronco,
                     arvore        = arvore,
                     estrada       = estrada,
                     carroDireito  = carroDireito,
                     carroEsquerda = carroEsquerda,
                     título        = título,
                     menu          = menu,
                     pausa         = pausa,
                     gameover      = gameover
                    }

{-|
Função define o ínicio do jogo quando o jogador começar a jogar, de acordo com o EstadoGloss definido.
-}
estadoGlossInicial ::Imagens -> EstadoGloss
estadoGlossInicial imagens = (Opcoes Jogar,estadoInicial, imagens)

{-|
Função que permite fazer a leitura do mapa do jogo e suas alterações.
-}
readMapa :: Mapa -> [(Terreno,[Obstaculo])]
readMapa (Mapa _ lista) = lista

{-|
Função que cria os vários menus, botões, imagens:
-}
desenhaEstadoGloss :: EstadoGloss -> IO Picture 
desenhaEstadoGloss t@(menu,(Jogo (Jogador (x,y)) mapa1, Parado, points, player),imagens) = return $ Pictures [drawState t]

-- -Menu principal
drawState :: EstadoGloss -> Picture
drawState (Opcoes Jogar,(Jogo (Jogador (x,y)) mapa1, jogada,points, player),imagens) = Pictures [(Translate 0 20 $ Scale 1 1 (menu imagens)),(Translate 0 70 $ Scale 3 3 (título imagens)),Color blue $ Translate 0 (-30) $ drawOption "Jogar", Translate 0 (-100) $ drawOption "Sair"]
drawState (Opcoes Sair,(Jogo (Jogador (x,y)) mapa1, jogada,points, player),imagens) = Pictures [(Translate 0 20 $ Scale 1 1 (menu imagens)),(Translate 0 70 $ Scale 3 3 (título imagens)), Translate 0 (-30) $ drawOption "Jogar", Color blue $ Translate 0 (-100) $ drawOption "Sair"]

-- -Tela de selecionar as personagens
drawState (Escolhe Sapo,(Jogo (Jogador (x,y)) mapa1, jogada, points, player),(imagens)) = Pictures [(Translate 0 20 $ Scale 1 1 (menu imagens)), Color blue $ Translate 0 10 $ drawOption "Sapo", Translate 0 (-50) $ drawOption "Abelha"]
drawState (Escolhe Abelha,(Jogo (Jogador (x,y)) mapa1, jogada, points, player),imagens) = Pictures [(Translate 0 20 $ Scale 1 1 (menu imagens)), Translate 0 10 $ drawOption "Sapo", Color blue $ Translate 0 (-50) $ drawOption "Abelha"]

-- -Tela quando se perde o jogo
drawState (ModoPerder,(Jogo (Jogador (x,y)) mapa1, jogada, points, player),imagens) = Pictures [(Translate 0 20 $ Scale 2 2 (gameover imagens)), Color blue $ Translate 200 (-10) $ drawOption "Menu"]

-- -Tela de pausa do jogo
drawState (Pausa,(Jogo (Jogador (x,y)) mapa1, jogada, points, player),imagens) = Pictures [(Translate 0 20 $ Scale 1 1 (pausa imagens)), Color blue $ Translate 100 (-10) $ drawOption "Continuar"]

-- -Mapa do jogo, obstáculos e o jogador
drawState (ModoJogo,(Jogo (Jogador (x,y)) mapa1, jogada,points, player ), imagens) =  Pictures (a ++ [b] ++ [c])
       where a = desenhamapa comprimento altura mapa imagens
             b = desenhaJogador (ModoJogo,(Jogo (Jogador (x,y)) mapa1, jogada,points, player),imagens)
             c = desenhapontuacao (ModoJogo,(Jogo (Jogador (x,y)) mapa1, jogada,points, player),imagens)
             mapa = readMapa mapa1

{-|
Função que define um tamanho aos botões.
-} 
drawOption option = Translate (-50) 0 $ Scale 0.5 0.5 $ Text option

{-|
Função que cria a pontuação do jogo.
-}
desenhapontuacao :: EstadoGloss -> Picture
desenhapontuacao (ModoJogo,(Jogo (Jogador (x,y)) mapa1, jogada, points, player),imagens) = Translate 200 200 $ Scale 0.3 0.3 $ Text ("Points:" ++ show points)

{-|
Função que cria o mapa do jogo.
-}
desenhamapa :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
desenhamapa x y (h:t) imagens = linha ++ resto 
       where linha = desenhalinha x y h imagens 
             resto = desenhamapa x (y-lado) t imagens
desenhamapa  _ _ _ _ = [] 

{-|
Função que cria uma linha de terreno e seus obstáculos no mapa.
-}
desenhalinha :: Float -> Float -> (Terreno,[Obstaculo]) -> Imagens -> [Picture]
desenhalinha x y (t,obs:o) imagens = bloco : resto 
       where bloco = desenhabloco x y (t,[obs]) imagens 
             resto = desenhalinha (x + lado) y (t,o) imagens 
desenhalinha  _ _ _ _ = [] 

{-|
Função que cria um bloco da linha de terreno do mapa.
-}
desenhabloco :: Float -> Float -> (Terreno,[Obstaculo]) -> Imagens -> Picture 
desenhabloco x y (Rio v,[obs]) imagens 
       | obs == Nenhum = translate x y (rio imagens)
       | otherwise     = translate x y (tronco imagens)

desenhabloco x y (Relva, [obs]) imagens 
       | obs == Arvore = translate x y (arvore imagens)
       | otherwise     = translate x y (relva imagens)


desenhabloco x y (Estrada v,[obs]) imagens 
       | obs == Carro && v > 0 = translate x y (carroEsquerda imagens)
       | obs == Carro && v < 0 = translate x y (carroDireito imagens)
       |otherwise              = translate x y (estrada imagens)

{-|
Função que cria o jogador dependendo da personagem que escolher.
-}
desenhaJogador :: EstadoGloss -> Picture
desenhaJogador (ModoJogo, (Jogo (Jogador (x,y)) mapa, jogada, points, Sapo), imagens) = Translate (fromIntegral (x*100-300)) (fromIntegral (-1*(y*100-300))) (frogback imagens)
desenhaJogador (ModoJogo, (Jogo (Jogador (x,y)) mapa, jogada, points, Abelha), imagens) = Translate (fromIntegral (x*100-300)) (fromIntegral (-1*(y*100-300))) (bee imagens)

{-|
Função que permite o funcionamento dos botões e suas funções:
-}
reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss

-- -Menu
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  (Opcoes Jogar,(Jogo (Jogador (x,y)) mapa , jogada, points, player), imagens) = return (Opcoes Sair,  (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (Opcoes Sair, (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)  = return (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)  (Opcoes Sair, (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)  = return (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa ,jogada,points, player), imagens)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (Opcoes Jogar,(Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)  = return (Opcoes Sair,  (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar,(Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)  = return (Escolhe Sapo, (Jogo (Jogador (x,y)) mapa ,jogada, points, Sapo), imagens)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)  = return (error "Fim de Jogo")
reageEventoGloss (EventKey (SpecialKey KeyEsc) Down _ _) (menu, (Jogo (Jogador (x,y)) mapa ,jogada, points, player), imagens)  = return (Opcoes Jogar, estadoInicial, imagens)

-- -Personagens
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Escolhe Sapo,(Jogo (Jogador (x,y)) mapa ,jogada, points, Sapo), imagens)  = return (Escolhe Abelha,(Jogo (Jogador (x,y)) mapa ,jogada, points, Abelha), imagens)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Escolhe Sapo,(Jogo (Jogador (x,y)) mapa ,jogada, points, Sapo), imagens)  = return (ModoJogo,(Jogo (Jogador (x,y)) mapa ,jogada, points, Sapo), imagens)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Escolhe Abelha,(Jogo (Jogador (x,y)) mapa ,jogada, points, Abelha), imagens)  = return (Escolhe Sapo,(Jogo (Jogador (x,y)) mapa ,jogada, points, Sapo), imagens)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Escolhe Abelha,(Jogo (Jogador (x,y)) mapa ,jogada, points, Abelha), imagens)  = return (ModoJogo,(Jogo (Jogador (x,y)) mapa ,jogada, points, Abelha), imagens)

-- -Tela de gameover
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _)  (ModoPerder,(Jogo (Jogador (x,y)) mapa , jogada, points, player), imagens) = return (Opcoes Jogar, estadoInicial, imagens)

-- -Pausa
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _)  (ModoJogo,(Jogo (Jogador (x,y)) mapa , jogada, points, player), imagens) = return (Pausa, (Jogo (Jogador (x,y)) mapa , jogada, points, player), imagens)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _)  (Pausa,(Jogo (Jogador (x,y)) mapa , jogada, points, player), imagens) = return (ModoJogo, (Jogo (Jogador (x,y)) mapa , jogada, points, player), imagens)

-- -Movimento do Jogador
reageEventoGloss k (ModoJogo,(Jogo (Jogador (x,y)) (Mapa z u) ,jogada, points, player), imagens) = case k of     
       (EventKey (SpecialKey KeyUp) Down _ _)     -> do 
             if (eRelva (fst (u !! (y-1)) ) == True) && (( snd( u !! (y-1)) !! (x)) == Arvore)
             then return (ModoJogo,(Jogo (Jogador (x,y)) (Mapa z u) ,Parado, (points+1), player), imagens)
             else return (ModoJogo,(Jogo (Jogador (x,y-1)) (Mapa z u) ,Parado, (points+1), player), imagens)
       (EventKey (SpecialKey KeyDown) Down _ _)   -> do 
             if (eRelva (fst (u !! (y+1)) ) == True) && (( snd( u !! (y+1)) !! (x)) == Arvore)
             then return (ModoJogo,(Jogo (Jogador (x,y)) (Mapa z u) ,Parado, (points-1), player), imagens)
             else return (ModoJogo,(Jogo (Jogador (x,y+1)) (Mapa z u) ,Parado, (points-1), player), imagens)
       (EventKey (SpecialKey KeyLeft) Down _ _)   -> do 
             if (eRelva (fst (u !! (y)) ) == True) && (( snd( u !! (y)) !! (x-1)) == Arvore)
             then return (ModoJogo,(Jogo (Jogador (x,y)) (Mapa z u) ,Parado, points, player), imagens)
             else return (ModoJogo,(Jogo (Jogador (x-1,y)) (Mapa z u) ,Parado, points, player), imagens)
       (EventKey (SpecialKey KeyRight) Down _ _)  -> do 
             if (eRelva (fst (u !! (y)) ) == True) && (( snd( u !! (y)) !! (x+1)) == Arvore)
             then return (ModoJogo,(Jogo (Jogador (x,y)) (Mapa z u) ,Parado, points, player), imagens)
             else return (ModoJogo,(Jogo (Jogador (x+1,y)) (Mapa z u) ,Parado, points, player), imagens)
       _ -> return (ModoJogo,(Jogo (Jogador (x,y)) (Mapa z u) ,jogada, points, player), imagens)

reageEventoGloss _ w = return w

{-|
Função que verifica se o terreno selecionado é relva ou não.
-}
eRelva :: Terreno -> Bool
eRelva (Relva) = True
eRelva (_) = False

{-|
Função que permite a criação das próximas linhas de terreno e obstáculos do mapa, o movimento do jogador, o movimento dos troncos e dos carros e que indica se o jogador perdeu ou não.
-}
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss _ (ModoJogo,(Jogo (Jogador (x,y)) mapa1 ,jogada, points, player), imagens)
   | jogoTerminou (Jogo (Jogador (x,y)) mapa1) = return (ModoPerder,(Jogo (Jogador (x,y)) mapa1, jogada, points, player),imagens)
   | otherwise = do
       n <- randomRIO (1,45) 
       return (ModoJogo,(deslizaJogo n (animaJogo (Jogo (Jogador (x,y)) mapa1) jogada),jogada, points, player), imagens)
reageTempoGloss _ p = return p

{-|
Função que cria o frame rate do jogo (o quão rápido a próxima linha de terreno deve aparecer) .
-}
fr :: Int
fr = 1

{-|
Função que permite o jogo ficar o ecrã inteiro aberto quando ligar o jogo.
-}
dm :: Display
dm = FullScreen

{-|
Função que permite colocar todas as peças a funcionar em conjunto.
-}
main :: IO ()
main = do 
    imagens <- carregarImagens
    playIO
        dm     
        white
        fr
        (estadoGlossInicial imagens)
        desenhaEstadoGloss
        reageEventoGloss
        reageTempoGloss
        

