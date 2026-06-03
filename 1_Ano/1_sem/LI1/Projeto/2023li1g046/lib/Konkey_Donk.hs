{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Redundant map" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Konkey_Donk where
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import EncontraPosicaoBlocos
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Niveis
import Jogador
import MovimentoInimigos (movimentoInimigoAleatorio)
import Data.List
import Data.IntMap (keys)


type Ticks = Int

--type Estado = ((Float,Float),[Picture],Maybe Acao,[Maybe Acao],Jogo)
type Estado = ((Float,Float),Jogo, [Picture],Int,[Key],Int)

estadoinicial ::  Int -> [Key]-> Int -> [Picture] ->  Estado
estadoinicial ticks keys nivel pics = ((0,0),nivel1,pics,ticks,keys,nivel)

ganhou :: Estado -> Bool
ganhou (_,Jogo (Mapa _ (x,y) _) _ _ j,_,_,_,_) = colideHitbox ((x-20,y+40),(x+20,y-40)) (hitbox j)

perdeu :: Estado -> Bool
perdeu (_,Jogo _ _ _ j,_,_,_,_) = getVida j <= 0

desenhamapa :: Estado -> Picture
desenhamapa e = Pictures (charParaPeca e ++ [desenhaJogador e] ++ desenhaColecionaveis e ++ [princesa e] ++ desenhaInimigos e ++ desenhaPontuacao e ++ desenhavidas e )

charParaPeca :: Estado -> [Picture]
charParaPeca (_,Jogo (Mapa _ _ []) _ _ _,_,_,_,_) = []
charParaPeca ((x,y),Jogo (Mapa a c ([]:t)) i col j,pic,ti,k,1) = charParaPeca ((x-fromIntegral (length (head t))*50,y-50),Jogo (Mapa a c t) i col j,pic,ti,k,1)
charParaPeca ((x,y),Jogo (Mapa a c ((b:bs):t)) i col j,pic,ti,k,1) |b == EscadaIncompleta = Translate x y  (pic !! 2) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,1)
                                                                   |b == Plataforma = Translate x y  (pic !! 1) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,1)
                                                                   |b == Escada = Translate x y  (pic !! 2) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,1)
                                                                   |b == Alcapao = Translate x y (head pic) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,1)
                                                                   |otherwise = Translate x y (pic !! 3) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,1)
charParaPeca ((x,y),Jogo (Mapa a c ([]:t)) i col j,pic,ti,k,2) = charParaPeca ((x-fromIntegral (length (head t))*50,y-50),Jogo (Mapa a c t) i col j,pic,ti,k,1)
charParaPeca ((x,y),Jogo (Mapa a c ((b:bs):t)) i col j,pic,ti,k,2) |b == EscadaIncompleta = Translate x y  (pic !! 2) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,2)
                                                                   |b == Plataforma = Translate x y  (pic !! 1) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,2)
                                                                   |b == Escada = Translate x y  (pic !! 2) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,2)
                                                                   |b == Alcapao = Translate x y (head pic) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,2)
                                                                   |otherwise = Translate x y (pic !! 3) : charParaPeca ((x+50,y),Jogo (Mapa a c (bs:t)) i col j,pic,ti,k,2)



desenhaestado :: Estado -> Picture
desenhaestado e@(p,j,pics,ticks,keys,n) | ganhou e = Translate 0 0 (pics !! 43)
desenhaestado e@(p,j,pics,ticks,keys,1) = desenhamapa ((-600,475),j,pics,ticks,keys,1)
desenhaestado e@(p,j,pics,ticks,keys,2) = desenhamapa ((-725,500),j,pics,ticks,keys,2)
desenhaestado (p,j,pics,t,k,0) = Translate 0 0 (pics !! 37)
desenhaestado (p,j,pics,t,k,3) = Translate 0 0 (pics !! 38)
desenhaestado (p,j,pics,t,k,4) = Translate 0 0 (pics !! 39)
desenhaestado (p,j,pics,t,k,5) = Translate 0 0 (pics !! 40)
desenhaestado (p,j,pics,t,k,6) = Translate 0 0 (pics !! 41)
desenhaestado (p,j,pics,t,k,7) = Translate 0 0 (pics !! 42)


escolheMenus :: Event -> Estado -> Estado
escolheMenus (EventKey (Char 'c') Down _ _) e@(p,j,pics,t,k,pt) |pt==0 = (p,j,pics,t,k,5)
                                                                |pt==5 = (p,nivel1,pics,t,k,1)
                                                                |pt==6 = (p,nivel2,pics,t,k,2)
escolheMenus (EventKey (Char 's') Down _ _) e@(p,j,pics,t,k,pt) |pt==0 = (p,j,pics,t,k,3)
                                                                |pt==3 = (p,j,pics,t,k,4)
                                                                |pt==4 = (p,j,pics,t,k,0)
escolheMenus (EventKey (Char 'w') Down _ _) e@(p,j,pics,t,k,pt) |pt==4 = (p,j,pics,t,k,3)
                                                                |pt==3 = (p,j,pics,t,k,0)
                                                                |pt==0 = (p,j,pics,t,k,4)
escolheMenus (EventKey (Char 'd') Down _ _) e@(p,j,pics,t,k,5) = (p,j,pics,t,k,6)
escolheMenus (EventKey (Char 'a') Down _ _) e@(p,j,pics,t,k,6) = (p,j,pics,t,k,5)
escolheMenus (EventKey (Char 'q') Down _ _) e@(p,j,pics,t,k,6) = (p,j,pics,t,k,0)
escolheMenus (EventKey (Char 'q') Down _ _) e@(p,j,pics,t,k,5) = (p,j,pics,t,k,0)
escolheMenus (EventKey (Char 'c') Down _ _) e@(p,j,pics,t,k,3) = (p,j,pics,t,k,7)
escolheMenus (EventKey (Char 'q') Down _ _) e@(p,j,pics,t,k,7) = (p,j,pics,t,k,3)
escolheMenus _ e = e

desenhaJogador :: Estado -> Picture
desenhaJogador (_,Jogo mapa _ _ jogador,pics,t,_,1) | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) && vx == 0 = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 18)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && vx == 0 = Translate (realToFrac x - 645) (realToFrac y + 515) (pics !! 24)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) && vx == 0 = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 19)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && vx == 0 = Translate (realToFrac x - 605) (realToFrac y + 515) (pics !! 25)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 0 || mod t 8 == 1) = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 14)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 2 || mod t 8 == 3) = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 16)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 0 || mod t 8 == 1) = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 15)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 2 || mod t 8 == 3)= Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 17)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 4 || mod t 8 == 5) = Translate (realToFrac x - 645) (realToFrac y + 515) (pics !! 20)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 = Translate (realToFrac x - 645) (realToFrac y + 515) (pics !! 22)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 4 || mod t 8 == 5) = Translate (realToFrac x - 615) (realToFrac y + 515) (pics !! 21)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 = Translate (realToFrac x - 615) (realToFrac y + 515) (pics !! 23)
                                                    | getDirecao jogador == Este && not (getEmEscada jogador) && vx == 0 = Translate (realToFrac x- 625) (realToFrac y + 500) (pics !! 9)
                                                    | getDirecao jogador == Oeste && not (getEmEscada jogador) && vx == 0 = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 8)
                                                    | (getEmEscada jogador || chegouAoFimDaEscada jogador mapa) && vy == 0 = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 27)
                                                    | getEmEscada jogador && vy /= 0 && (mod t 8 == 0 || mod t 8 == 1 || mod t 8 == 2 || mod t 8 == 3) = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 28)
                                                    | getEmEscada jogador && vy /= 0 = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 29)
                                                    | getDirecao jogador == Oeste && not (getEmEscada jogador) && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) = Translate (realToFrac x- 625) (realToFrac y + 500) (pics !! 10)
                                                    | getDirecao jogador == Este && not (getEmEscada jogador) && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) = Translate (realToFrac x- 625) (realToFrac y + 500) (pics !! 11)
                                                    | getDirecao jogador == Oeste && not (getEmEscada jogador) = Translate (realToFrac x- 625) (realToFrac y + 500) (pics !! 12)
                                                    | getDirecao jogador == Este && not (getEmEscada jogador) = Translate (realToFrac x- 625) (realToFrac y + 500) (pics !! 13)
                                                 
    where (vx,vy) = getVelocidade jogador
          (x,y) = getPosicao jogador
desenhaJogador (_,Jogo mapa _ _ jogador,pics,t,_,2) | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) && vx == 0= Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 18)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 6 == 3 || mod t 6 == 4 || mod t 6 == 5) && vx == 0 = Translate (realToFrac x - 770) (realToFrac y + 540) (pics !! 24)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) && vx == 0 = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 19)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 6 == 3 || mod t 6 == 4 || mod t 6 == 5) && vx == 0 = Translate (realToFrac x - 730) (realToFrac y + 540) (pics !! 25)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 0 || mod t 8 == 1) = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 14)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 2 || mod t 8 == 3) = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 16)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 0 || mod t 8 == 1) = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 15)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 2 || mod t 8 == 3)= Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 17)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 4 || mod t 8 == 5) = Translate (realToFrac x - 770) (realToFrac y + 540) (pics !! 20)
                                                    | getDirecao jogador == Este && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 = Translate (realToFrac x - 770) (realToFrac y + 540) (pics !! 22)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 && (mod t 8 == 4 || mod t 8 == 5) = Translate (realToFrac x - 740) (realToFrac y + 540) (pics !! 21)
                                                    | getDirecao jogador == Oeste && fst (getAplicaDano jogador) && snd (getAplicaDano jogador) > 0 = Translate (realToFrac x - 740) (realToFrac y + 540) (pics !! 23)
                                                    | (getEmEscada jogador || chegouAoFimDaEscada jogador mapa) && vy == 0 = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 27)
                                                    | getEmEscada jogador && vy /= 0 && (mod t 8 == 0 || mod t 8 == 1 || mod t 8 == 2 || mod t 8 == 3) = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 28)
                                                    | getEmEscada jogador && vy /= 0 = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 29)
                                                    | getDirecao jogador == Este && not (getEmEscada jogador) && vx == 0 = Translate (realToFrac x- 750) (realToFrac y + 525) (pics !! 9)
                                                    | getDirecao jogador == Oeste && not (getEmEscada jogador) && vx == 0 = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 8)
                                                    | getDirecao jogador == Oeste && not (getEmEscada jogador) && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) = Translate (realToFrac x- 750) (realToFrac y + 525) (pics !! 10)
                                                    | getDirecao jogador == Este && not (getEmEscada jogador) && (mod t 6 == 0 || mod t 6 == 1 || mod t 6 == 2) = Translate (realToFrac x- 750) (realToFrac y + 525) (pics !! 11)
                                                    | getDirecao jogador == Oeste && not (getEmEscada jogador) = Translate (realToFrac x- 750) (realToFrac y + 525) (pics !! 12)
                                                    | getDirecao jogador == Este && not (getEmEscada jogador) = Translate (realToFrac x- 750) (realToFrac y + 525) (pics !! 13)
                                                 
                                                 
        where (vx,vy) = getVelocidade jogador
              (x,y) = getPosicao jogador


desenhaColecionaveis :: Estado -> [Picture]
desenhaColecionaveis (_,Jogo _ _ [] _,_,_,_,_) = []
desenhaColecionaveis (p,Jogo m i (col:cols) j,pics,ti,k,1) | tipo == Martelo = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 35) : desenhaColecionaveis (p,Jogo m i cols j,pics,ti,k,1)
                                                           | otherwise = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 36) : desenhaColecionaveis (p,Jogo m i cols j,pics,ti,k,1)
    where (tipo,(x,y)) = col
desenhaColecionaveis (p,Jogo m i (col:cols) j,pics,ti,k,2) | tipo == Martelo = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 35) : desenhaColecionaveis (p,Jogo m i cols j,pics,ti,k,2)
                                                           | otherwise = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 36) : desenhaColecionaveis (p,Jogo m i cols j,pics,ti,k,2)
    where (tipo,(x,y)) = col

desenhaPontuacao :: Estado -> [Picture]
desenhaPontuacao (_,Jogo _ _ _ jogador,pics,_,_,_) | getPontos jogador == 0 = []
                                                   | getPontos jogador == 100 = [Translate 700 500 (pics !! 36)]
                                                   | getPontos jogador == 200 = [Translate 700 500 (pics !! 36)] ++ [Translate 775 500 (pics !! 36)]
                                                   | getPontos jogador == 300 = [Translate 700 500 (pics !! 36)] ++ [Translate 775 500 (pics !! 36)] ++ [Translate 850 500 (pics !! 36)]


desenhavidas :: Estado -> [Picture]
desenhavidas (_,Jogo _ _ _ jogador,pics,_,_,_) | getVida jogador == 0 = [] 
                                               | getVida jogador == 1 || getVida jogador == 2 = [Translate (-850) 500 (pics !! 48)] ++ [Translate (-775) 500 (pics !! 49)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 3 || getVida jogador == 4 = [Translate (-850) 500 (pics !! 47)] ++ [Translate (-775) 500 (pics !! 49)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 5 || getVida jogador == 6 = [Translate (-850) 500 (pics !! 46)] ++ [Translate (-775) 500 (pics !! 49)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 7 || getVida jogador == 8 = [Translate (-850) 500 (pics !! 45)] ++ [Translate (-775) 500 (pics !! 49)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 10 || getVida jogador == 9 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 49)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 11 || getVida jogador == 12 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 48)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 13 || getVida jogador == 14 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 47)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 15 || getVida jogador == 16 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 46)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 17 || getVida jogador == 18 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 45)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 19 || getVida jogador == 20 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 44)] ++ [Translate (-700) 500 (pics !! 49)]
                                               | getVida jogador == 21 || getVida jogador == 22 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 44)] ++ [Translate (-700) 500 (pics !! 48)]
                                               | getVida jogador == 23 || getVida jogador == 24 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 44)] ++ [Translate (-700) 500 (pics !! 47)]
                                               | getVida jogador == 25 || getVida jogador == 26 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 44)] ++ [Translate (-700) 500 (pics !! 46)]
                                               | getVida jogador == 27 || getVida jogador == 28 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 44)] ++ [Translate (-700) 500 (pics !! 45)]
                                               | getVida jogador == 29 || getVida jogador == 30 = [Translate (-850) 500 (pics !! 44)] ++ [Translate (-775) 500 (pics !! 44)] ++ [Translate (-700) 500 (pics !! 44)]


desenhaInimigos:: Estado -> [Picture]
desenhaInimigos (_,Jogo _ [] _ _,pics,_,_,_) = []
desenhaInimigos (p,Jogo m (i:is) c j,pics,ti,k,1)   | getTipo i == MacacoMalvado && (mod ti 20 == 5 || mod ti 20 == 6 || mod ti 20 == 7 || mod ti 20 == 8 || mod ti 20 == 9 || mod ti 20 == 0 || mod ti 20 == 1 || mod ti 20 == 2 || mod ti 20 == 3 || mod ti 20 == 4) = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 4) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,1)
                                                    | getTipo i == MacacoMalvado = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 5) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,1)
                                                    | getTipo i == Fantasma && (mod ti 18 == 0 || mod ti 18 == 1 || mod ti 18 == 2 || mod ti 18 == 3 || mod ti 18 == 4 || mod ti 18 == 5 || mod ti 18 == 6 || mod ti 18 == 7 || mod ti 18 == 8) = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 33) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,1)
                                                    | otherwise = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 32) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,1)
    where (x,y) = getPosicao i
desenhaInimigos (p,Jogo m (i:is) c j,pics,ti,k,2)   | getTipo i == MacacoMalvado && (mod ti 20 == 5 || mod ti 20 == 6 || mod ti 20 == 7 || mod ti 20 == 8 || mod ti 20 == 9 || mod ti 20 == 0 || mod ti 20 == 1 || mod ti 20 == 2 || mod ti 20 == 3 || mod ti 20 == 4) = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 4) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,2)
                                                    | getTipo i == MacacoMalvado = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 5) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,2)
                                                    | getTipo i == Fantasma && (mod ti 18 == 0 || mod ti 18 == 1 || mod ti 18 == 2 || mod ti 18 == 3 || mod ti 18 == 4 || mod ti 18 == 5 || mod ti 18 == 6 || mod ti 18 == 7 || mod ti 18 == 8) = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 33) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,2)
                                                    | otherwise = Translate (realToFrac x - 750) (realToFrac y + 525) (pics !! 32) : desenhaInimigos (p,Jogo m is c j,pics,ti,k,2)
    where (x,y) = getPosicao i


princesa :: Estado -> Picture
princesa (p,Jogo (Mapa _ (x,y) _) _ _ _,pics,ti,k,1) | ti /= 0 && (mod ti 50 == 40 || mod ti 50 == 41 || mod ti 50 == 42 || mod ti 50 == 43 || mod ti 50 == 44 || mod ti 50 == 45 || mod ti 50 == 46 || mod ti 50 == 47 || mod ti 50 == 48 ) = Translate (realToFrac x - 592.5) (realToFrac y + 504.5) (pics !! 31)
                                                     | otherwise = Translate (realToFrac x - 625) (realToFrac y + 500) (pics !! 30)
princesa (p,Jogo (Mapa _ (x,y) _) _ _ _,pics,ti,k,2) | ti /= 0 && (mod ti 50 == 40 || mod ti 50 == 41 || mod ti 50 == 42 || mod ti 50 == 43 || mod ti 50 == 44 || mod ti 50 == 45 || mod ti 50 == 46 || mod ti 50 == 47 || mod ti 50 == 48 ) = Translate (realToFrac x - 692.5) (realToFrac y + 529.5) (pics !! 31)
                                                     | otherwise = Translate (realToFrac x - 725) (realToFrac y + 525) (pics !! 30)

reageEvento :: Event -> Estado -> Estado
reageEvento evento e@(_,_,_,_,_,0) = escolheMenus evento e
reageEvento evento e@(_,_,_,_,_,3) = escolheMenus evento e
reageEvento evento e@(_,_,_,_,_,4) = escolheMenus evento e
reageEvento evento e@(_,_,_,_,_,5) = escolheMenus evento e
reageEvento evento e@(_,_,_,_,_,6) = escolheMenus evento e
reageEvento evento e@(_,_,_,_,_,7) = escolheMenus evento e
reageEvento evento (p,jogo,pics,ti,keys,n) = (p,jogo,pics,ti, movimentoFluido evento keys,n)
reageEvento _ e = e

reageTempo :: Float -> Estado -> Estado
reageTempo n e@(p,Jogo mapa ini c joga, pics,ti,keys,1) | ganhou e || perdeu e = (p,Jogo mapa ini c joga, pics,ti,[],0)
                                                        | dano && tempo > 0 = (p,reageKeys keys ti (movimenta ti (realToFrac ti) (atualiza (concatMap (movimentoInimigoAleatorio ti mapa) ini)  Nothing (Jogo mapa ini c (setAplicaDano (dano,tempo-0.1) joga)))),pics,ti+1,keys,1)
                                                        | otherwise = (p,reageKeys keys ti  (movimenta ti (realToFrac ti) (atualiza (concatMap (movimentoInimigoAleatorio ti mapa) ini)  Nothing (Jogo mapa ini c joga))) ,pics,ti+1,keys,1)
     where (dano,tempo) = getAplicaDano joga
reageTempo n e@(p,Jogo mapa ini c joga, pics,ti,keys,2) | ganhou e || perdeu e = (p,Jogo mapa ini c joga, pics,ti,[],0)
                                                        | dano && tempo > 0 = (p,reageKeys keys ti (movimenta ti (realToFrac ti) (atualiza (concatMap (movimentoInimigoAleatorio ti mapa) ini) Nothing (Jogo mapa ini c (setAplicaDano (dano,tempo-0.1) joga)))) ,pics,ti+1,keys,2)
                                                        | otherwise = (p,reageKeys keys ti (movimenta ti (realToFrac ti) (atualiza (concatMap (movimentoInimigoAleatorio ti mapa) ini)  Nothing (Jogo mapa ini c joga))) ,pics,ti+1,keys,2)
    where (dano,tempo) = getAplicaDano joga
reageTempo n (p,j,pics,ti,k,ni) = (p,j,pics,ti+1,k,ni)

reageKeys :: [Key] -> Int -> Jogo -> Jogo
reageKeys [] ti j = j
reageKeys (key:keys) ti j = reageKeys keys ti (movePlayer key ti j)

movePlayer (Char 'd') ti (Jogo mapa ini c player) = movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just AndarDireita) player))
movePlayer (Char 'w') ti (Jogo mapa ini c player) = movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just Subir) player))
movePlayer (Char 'a') ti (Jogo mapa ini c player) = movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just AndarEsquerda) player))
movePlayer (Char 's') ti (Jogo mapa ini c player) = movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just Descer) player))
movePlayer (SpecialKey KeySpace) ti (Jogo mapa ini c player) = movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just Saltar) player))
movePlayer _ _ jogo = jogo

movimentoFluido :: Event -> [Key] -> [Key]
movimentoFluido (EventKey key Up _ _) = delete key
movimentoFluido (EventKey key Down _ _) = (:) key
movimentoFluido _ = id

acaoJogador :: Key -> Estado -> Estado
acaoJogador (Char 'w') (p,Jogo mapa ini c jogador, pics,ti,k,1) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Subir) jogador)), pics,ti,k,1)
acaoJogador (Char 's') (p,Jogo mapa ini c jogador,pics,ti,k,1) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Descer) jogador)), pics,ti,k,1)
acaoJogador (Char 'a') (p,Jogo mapa ini c jogador, pics,ti,k,1) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just AndarEsquerda) jogador)), pics,ti,k,1)
acaoJogador (Char 'd') (p,Jogo mapa ini c jogador, pics,ti,k,1) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just AndarDireita) jogador)), pics,ti,k,1)
acaoJogador (SpecialKey KeySpace) (p,Jogo mapa ini c jogador,pics,ti,k,1) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Saltar) jogador)), pics,ti,k,1)
acaoJogador _ (p,Jogo mapa ini c jogador,pics,ti,k,1) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Parar) jogador)), pics,ti,k,1)
acaoJogador (Char 'w') (p,Jogo mapa ini c jogador, pics,ti,k,2) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Subir) jogador)), pics,ti,k,2)
acaoJogador (Char 's') (p,Jogo mapa ini c jogador,pics,ti,k,2) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Descer) jogador)), pics,ti,k,2)
acaoJogador (Char 'a') (p,Jogo mapa ini c jogador, pics,ti,k,2) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just AndarEsquerda) jogador)), pics,ti,k,2)
acaoJogador (Char 'd') (p,Jogo mapa ini c jogador, pics,ti,k,2) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c  (aplicaAcao mapa (Just AndarDireita) jogador)), pics,ti,k,2)
acaoJogador (SpecialKey KeySpace) (p,Jogo mapa ini c jogador,pics,ti,k,2) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Saltar) jogador)), pics,ti,k,2)
acaoJogador _ (p,Jogo mapa ini c jogador,pics,ti,k,2) = (p,movimentaJogador ti (realToFrac ti) (Jogo mapa ini c (aplicaAcao mapa (Just Parar) jogador)), pics,ti,k,2)

acoesJogador :: [Key] -> Estado -> Estado
acoesJogador [] e = e
acoesJogador (h:t) e = acoesJogador t (acaoJogador h e)

fr :: Int
fr = 10

dm :: Display
dm = FullScreen

main :: IO ()
main = do
    alcapao <- loadBMP "images/alcapao.bmp"                                                    -- 0                
    plataforma <- loadBMP "images/plataforma.bmp"                                              -- 1 
    escada <- loadBMP "images/escada.bmp"                                                      -- 2
    vazio <- loadBMP "images/vazio.bmp"                                                        -- 3
    macaco1 <- loadBMP "images/macaco1.bmp"                                                    -- 4
    macaco2 <- loadBMP "images/macaco2.bmp"                                                    -- 5
    macacoperdeu <- loadBMP "images/macacoperdeu.bmp"                                          -- 6
    macacoperdeu2 <- loadBMP "images/macacoperdeu2.bmp"                                        -- 7
    marioparadoesquerda <- loadBMP "images/marioparadoesquerda.bmp"                            -- 8
    marioparadodireita <- loadBMP "images/marioparadodireita.bmp"                              -- 9
    marioandaresquerda1 <- loadBMP "images/marioandar1esquerda.bmp"                            -- 10
    marioandardireita1 <- loadBMP "images/marioandar1direita.bmp"                              -- 11
    marioandaresquerda2 <- loadBMP "images/marioandar2esquerda.bmp"                            -- 12  
    marioandardireita2 <- loadBMP "images/marioandar2direita.bmp"                              -- 13
    mariomartelobaixoandardireita1 <- loadBMP "images/mariomartelobaixoandar1direita.bmp"      -- 14
    mariomartelobaixoandaresquerda1 <- loadBMP "images/mariomartelobaixoandar1esquerda.bmp"    -- 15
    mariomartelobaixoandardireita2 <- loadBMP "images/mariomartelobaixoandar2direita.bmp"      -- 16
    mariomartelobaixoandaresquerda2 <- loadBMP "images/mariomartelobaixoandar2esquerda.bmp"    -- 17
    mariomartelobaixoparadodireita <- loadBMP "images/mariomartelobaixoparadodireita.bmp"      -- 18
    mariomartelobaixoparadoesquerda <- loadBMP "images/mariomartelobaixoparadoesquerda.bmp"    -- 19
    mariomartelocimaandardireita1 <- loadBMP "images/mariomartelocimaandar1direita.bmp"        -- 20
    mariomartelocimaandaresquerda1 <- loadBMP "images/mariomartelocimaandar1esquerda.bmp"      -- 21
    mariomartelocimanadardireita2 <- loadBMP "images/mariomartelocimaandar2direita.bmp"        -- 22
    mariomartelocimaandaresquerda2 <- loadBMP "images/mariomartelocimaandar2esquerda.bmp"      -- 23 
    mariomartelocimaparadodireita <- loadBMP "images/mariomartelocimaparadodireita.bmp"        -- 24
    mariomartelocimaparadoesquerda <- loadBMP "images/mariomartelocimaparadoesquerda.bmp"      -- 25
    mariomorto <- loadBMP "images/mariomorto.bmp"                                              -- 26
    mariofrentedaescada <- loadBMP "images/mariofrentedaescada.bmp"                            -- 27
    mariosubirescada1 <- loadBMP "images/mariosubirescada.bmp"                                 -- 28
    mariosubirescada2 <- loadBMP "images/mariosubirescada2.bmp"                                -- 29
    princesa <- loadBMP "images/princesa.bmp"                                                  -- 30
    princesaajuda <- loadBMP "images/princesaajuda.bmp"                                        -- 31
    inimigo1 <- loadBMP "images/inimigo1.bmp"                                                  -- 32
    inimigo2 <- loadBMP "images/inimigo2.bmp"                                                  -- 33
    inimigomorto <- loadBMP "images/inimigomorto.bmp"                                          -- 34
    martelo <- loadBMP "images/martelo.bmp"                                                    -- 35
    moeda <- loadBMP "images/moeda.bmp"                                                        -- 36
    menu0 <- loadBMP "images/play.bmp"                                                         -- 37
    menu3 <- loadBMP "images/info.bmp"                                                         -- 38
    menu4 <- loadBMP "images/quit.bmp"                                                         -- 39
    menu5 <- loadBMP "images/nivel1.bmp"                                                       -- 40
    menu6 <- loadBMP "images/nivel2.bmp"                                                       -- 41
    menu7 <- loadBMP "images/infoBox.bmp"                                                      -- 42
    ganhou <- loadBMP "images/ganhou.bmp"                                                      -- 43
    coracaocheio <- loadBMP "images/coracaocheio.bmp"                                          -- 44
    coracao80 <- loadBMP "images/coracao80.bmp"                                                -- 45
    coracao60  <- loadBMP "images/coracao60.bmp"                                               -- 46
    coracao40 <- loadBMP "images/coracao40.bmp"                                                -- 47
    coracao20 <- loadBMP "images/coracao20.bmp"                                                -- 48
    coracaovazio <- loadBMP "images/coracaovazio.bmp"                                          -- 49
    play dm
            black
            fr
            (estadoinicial 0 [] 0 [alcapao,plataforma,escada,vazio,macaco1,macaco2,macacoperdeu,macacoperdeu2,marioparadoesquerda,marioparadodireita,marioandaresquerda1,marioandardireita1,marioandaresquerda2,marioandardireita2,mariomartelobaixoandardireita1,mariomartelobaixoandaresquerda1,mariomartelobaixoandardireita2,mariomartelobaixoandaresquerda2,mariomartelobaixoparadodireita,mariomartelobaixoparadoesquerda,mariomartelocimaandardireita1,mariomartelocimaandaresquerda1,mariomartelocimanadardireita2,mariomartelocimaandaresquerda2,mariomartelocimaparadodireita,mariomartelocimaparadoesquerda,mariomorto,mariofrentedaescada,mariosubirescada1,mariosubirescada2,princesa,princesaajuda,inimigo1,inimigo2,inimigomorto,martelo,moeda,menu0,menu3,menu4,menu5,menu6,menu7,ganhou,scale 2 2 $ coracaocheio,scale 2 2 $ coracao80,scale 2 2 $ coracao60,scale 2 2 $ coracao40,scale 2 2 $ coracao20,scale 2 2 $ coracaovazio])
            desenhaestado
            reageEvento
            reageTempo

