module Jogador where

import Decuple
import LI12324
import Functions 
import EncontraPosicaoBlocos

inPlayer (velocidade, tipo, posicao, direcao, tamanho, emEscada, ressalta, vida, pontos, aplicaDano) = Personagem velocidade tipo posicao direcao tamanho emEscada ressalta vida pontos aplicaDano
outPlayer (Personagem velocidade tipo posicao direcao tamanho emEscada ressalta vida pontos aplicaDano) = (velocidade, tipo, posicao, direcao, tamanho, emEscada, ressalta, vida, pontos, aplicaDano)

jogadorteste = Personagem (2,2) Jogador (150,-75) Este (10,10) False False 10 5 (False,0)

jogadorteste2 = Personagem (2,2) Jogador (500,-50) Este (10,10) True False 0 5 (True,5)

jogadorteste3 = Personagem (5,5) Jogador (20,-40) Oeste (15,15) True False 3 50 (True,7)

jogadorteste4 = Personagem (5,5) Jogador (100,-100) Oeste (15,15) True False 3 20 (False,0)

inimigosteste = [Personagem (2,2) MacacoMalvado (20,-50) Este (10,10) False True 10 5 (False,0), Personagem (2,2) Fantasma (50,-50) Este (10,10) False True 10 5 (False,0),Personagem (2,2) MacacoMalvado (50,-50) Este (10,10) False True 10 5 (False,0),Personagem (2,2) Fantasma (10,-50) Este (10,10) False True 10 5 (False,0),Personagem (2,2) MacacoMalvado (70,-50) Este (10,10) False True 10 5 (False,0)]

jogoteste1 = Jogo mapateste [Personagem (2,2) MacacoMalvado (50,-100) Este (10,10) False True 10 5 (False,0),Personagem (2,2) MacacoMalvado (100,-250) Este (10,10) False True 10 5 (False,0)] [] jogadorteste4

jogoteste2 = Jogo mapateste [Personagem (2,2) MacacoMalvado (20,-30) Este (10,10) False True 10 5 (False,0),Personagem (2,2) MacacoMalvado (50,-60) Este (10,10) False True 10 5 (False,0)] [(Martelo,(10,-10))] jogadorteste3

jogomatainimigo = Jogo mapa1 [Personagem (0,0) Fantasma (160,-940) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (325,-290) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (1000,-490) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (800,-90) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) MacacoMalvado (275,-25) Oeste (128,150) False True 1 0 (False,0.0)] [(Moeda,(125,-440)),(Moeda,(1125,-875)),(Moeda,(1125,-40)),(Martelo,(275,-230))] (Personagem (0,0) Jogador (125,-915) Este (40,70) False False 30 0 (True,10))

jogonovazio = Jogo mapa1 [Personagem (0,0) Fantasma (160,-940) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (325,-290) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (1000,-490) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (800,-90) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) MacacoMalvado (275,-25) Oeste (128,150) False True 1 0 (False,0.0)] [(Moeda,(125,-440)),(Moeda,(1125,-875)),(Moeda,(1125,-40)),(Martelo,(275,-230))] (Personagem (0,0) Jogador (555,-700) Este (40,70) False False 30 0 (True,10))

jogosofredano = Jogo mapa1 [Personagem (0,0) Fantasma (555,-700) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (325,-290) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (1000,-490) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (800,-90) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) MacacoMalvado (275,-25) Oeste (128,150) False True 1 0 (False,0.0)] [(Moeda,(125,-440)),(Moeda,(1125,-875)),(Moeda,(1125,-40)),(Martelo,(275,-230))] (Personagem (0,0) Jogador (555,-700) Este (40,70) False False 30 0 (False,0.0))
mapPlayer f = inPlayer . f . outPlayer
--mapPlayer (uncurry put0 (split id ((+1) . get0)) player
--mapPlayer (ap0 ((+1) . get0)) player


getVelocidade = Decuple.get0 . outPlayer
getTipo = Decuple.get1 . outPlayer
getPosicao = Decuple.get2 . outPlayer
getDirecao = Decuple.get3 . outPlayer
getTamanho = Decuple.get4 . outPlayer
getEmEscada = Decuple.get5 . outPlayer
getRessalta = Decuple.get6 . outPlayer
getVida = Decuple.get7 . outPlayer
getPontos = Decuple.get8 . outPlayer
getAplicaDano = Decuple.get9 . outPlayer

setVelocidade x p = inPlayer $ Decuple.set0 x (outPlayer p) 
setTipo x p = inPlayer $ Decuple.set1 x (outPlayer p) 
setPosicao x p = inPlayer $ Decuple.set2 x (outPlayer p) 
setDirecao x p = inPlayer $ Decuple.set3 x (outPlayer p) 
setTamanho x p = inPlayer $ Decuple.set4 x (outPlayer p) 
setEmEscada x p = inPlayer $ Decuple.set5 x (outPlayer p) 
setRessalta x p = inPlayer $ Decuple.set6 x (outPlayer p)  
setVida x p = inPlayer $ Decuple.set7 x (outPlayer p) 
setPontos x p = inPlayer $ Decuple.set8 x (outPlayer p) 
setAplicaDano x p = inPlayer $ Decuple.set9 x (outPlayer p) 

apVelocidade f p = inPlayer . ap0 f . outPlayer 
apPosicao f = ap2 f . outPlayer
apDirecao f = inPlayer . ap3 f . outPlayer
apTamanho f = Decuple.ap4 f . outPlayer
apEmEscada f = Decuple.ap5 f . outPlayer
apRessalta f = Decuple.ap6 f . outPlayer 
apVida f = inPlayer . ap7 f . outPlayer
apPontos f = Decuple.ap8 f . outPlayer
apAplicaDano f = Decuple.ap9 f . outPlayer

hitboxmedidas :: (Double,Double) -> (Double,Double) -> Hitbox
hitboxmedidas (x,y) (w,h) = ((x - w/2, y + h/2), (x + w/2, y - h/2))

hitboxDano :: Personagem -> Direcao -> Hitbox
hitboxDano p Este = ((x + w/2, y + h/2), (x + w * 3/2, y - h/2))
    where (x,y) = getPosicao p 
          (w,h) = getTamanho p 
hitboxDano p Oeste = ((x - w * 3/2, y + h/2), (x - w/2, y - h/2))
    where (x,y) = getPosicao p 
          (w,h) = getTamanho p 

limitesHitboxPersonagem :: Personagem -> ((Double,Double),(Double,Double))
limitesHitboxPersonagem pers = ((x-w/2,x+w/2),(y-h/2,y+h/2))
    where (x,y) = getPosicao pers
          (w,h) = getTamanho pers


