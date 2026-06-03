module Niveis where
import LI12324 
import EncontraPosicaoBlocos
import Jogador 


nivel1 :: Jogo 
nivel1 = Jogo mapa1 [Personagem (0,0) Fantasma (925,-940) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (325,-290) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (1000,-490) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (800,-90) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) MacacoMalvado (275,-25) Oeste (128,150) False True 1 0 (False,0.0)] [(Moeda,(125,-440)),(Moeda,(1125,-875)),(Moeda,(1125,-40)),(Martelo,(275,-230))] (Personagem (0,0) Jogador (125,-915) Este (40,70) False False 30 0 (False,0.0))
tamanhoNivel1 :: (Float,Float)
tamanhoNivel1 = (-600,475)

nivel2 :: Jogo 
nivel2 = Jogo mapa2 [Personagem (0,0) Fantasma (500,-290) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (400,-540) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (1100,-390) Este (20,20) False True 1 0 (False,0.0),Personagem (0,0) Fantasma (900,-990) Oeste (20,20) False True 1 0 (False,0.0),Personagem (0,0) MacacoMalvado (1150,-625) Oeste (128,150) False True 1 0 (False,0.0)] [(Moeda,(700,-480)),(Moeda,(820,-800)),(Moeda,(1420,-350)),(Martelo,(250,-90))] (Personagem (0,0) Jogador (75,-965) Este (40,70) False False 30 0 (False,0.0))