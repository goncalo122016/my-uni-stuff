{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use any" #-}
{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Gonçalo José Vieira de Castro <a107337@alunos.uminho.pt>
              Luis Miguel Jeronimo Felicio <a106913@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324 
import Graphics.Gloss
import Jogador
import Functions
import EncontraPosicaoBlocos 

{-| A função colisoesParede recebe um Mapa e um Personagem e tem como objetivo testar se o Personagem se encontra dentro dos limites laterais (e cima e baixo)
ou se este se encontra em interação com as Plataformas do Mapa. Para isso usamos o conceito de HitBox tanto do Personagem como dos Blocos do tipo Plataforma.

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

== Exemplos

>>> colisoesParede (Mapa ((0,0),Este) (0,50) mapaexem) (Personagem (0,0) Jogador (300,0) Este (12.5,12.5) False False 3 100 (False,0)) 
    = True (O Personagem colide com os limites do mapa - neste caso ultrapassa o limite direito)

>>> colisoesParede (Mapa ((0,0),Este) (0,50) mapaexem) (Personagem (0,0) Jogador (10,30) Este (12.5,12.5) False False 3 100 (False,0)) 
    = False (O Personagem encontra-se dentro dos limites do mapa - não colide com nenhum limite)

>>> colisoesParede (Mapa ((0,0),Este) (0,50) mapaexem) (Personagem (0,0) Jogador (70,-50) Este (12.5,12.5) False False 3 100 (False,0))
    = True (O Personagem encontra-se dentro dos limites do mapa, mas colide com uma Plataforma)
-}

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m@(Mapa _ _ blo) p = (fst . fst $ hitbox p) <= 0 || ((fst . snd $ hitbox p) >= (50 * fromIntegral (length (head blo)))) || (snd . snd $ hitbox p) < ((-50) * (fromIntegral (length blo) - 1)) 

{-| Por outro lado a função colisoesPersonagens verifica, dadas duas personagens distintas, se as duas personagens colidem, ou mais especificamente se as suas
respetivas HitBoxes se intersetam.

== Exemplos

>>> colisoesPersonagens (Personagem (0,0) Fantasma (10,0) Oeste (40,40) False False 3 100 (False,0)) 
                        (Personagem (0,0) Jogador (30,0) Este (40,70) False False 3 100 (False,0)) 
    = True (Os Personagens efetivamente colidem - as suas HitBoxes intersetam-se)

>>> colisoesPersonagens (Personagem (0,0) Fantasma (100,0) Oeste (40,40) False False 3 100 (False,0)) 
                        (Personagem (0,0) Jogador (30,0) Este (40,70) False False 3 100 (False,0))  
    = False (Os Personagens não colidem - as suas HitBoxes não se cruzam)
-}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p0 p1 = colideHitbox hbp0 hbp1
      where
        hbp0 = hitbox p0
        hbp1 = hitbox p1

{-| Esta função hitbox é auxiliar às duas funções descritas acima. Dada uma Personagem a função devolve uma Hitbox do tipo (Posicao, Posicao) onde o primeiro 
ponto é o canto superior esquerdo e o segundo é o canto inferior direito, isto tudo tento em conta o Tamanho e a Posição da Personagem. 

== Exemplos

>>> hitbox (Personagem (0,0) Jogador (30,0) Este (40,70) False False 3 100 (False,0)) 
    = ((10.0,35.0),(50.0,-35.0))

>>> hitbox (Personagem (0,0) Fantasma (1,0) Oeste (40,40) False False 3 100 (False,0))  
    = ((-19.0,20.0),(21.0,-20.0))
-}

hitbox :: Personagem -> Hitbox
hitbox p = ((x - c/2, y + l/2), (x + c/2, y - l/2)) -- Hitbox onde o primeiro ponto é o canto superior esquerdo e o segundo é o canto inferior direito
  where (x, y) = getPosicao p
        (c, l) = getTamanho p

{-| A função colideHitbox é auxiliar à função colidePersonagens. Dadas duas HitBoxes a função "decide" se estas se intersetam de algum modo. Estão incluídos 
casos como: os retangulos(HitBoxes) tocam pelos cantos, os retangulos intersetam-se pelos lados, ou até por exemplo se os retangulos se sobrepõem completamente. 

== Exemplos

>>> colideHitbox ((0,0),(10,-10)) ((11,-11),(20,-20))
    = False (As Hitboxes (e respetivamente os retangulos por elas formadas) não se cruzam)

>>> colideHitbox ((0,0),(10,-10)) ((5,-5),(20,-20))
    = True (As Hitboxes (e respetivamente os retangulos por elas formadas) efetivamente se intersetam - o canto superior esquerdo da segunda está dentro da primeira)
-}

colideHitbox :: Hitbox -> Hitbox -> Bool
colideHitbox ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = (x1 <= x3 && x2 >= x3 && y1 >= y3 && y2 <= y3) || (x1 <= x4 && x2 >= x4 && y1 >= y4 && y2 <= y4) || (x3 <= x1 && x4 >= x1 && y3 >= y1 && y4 <= y1) || (x3 <= x2 && x4 >= x2 && y3 >= y2 && y4 <= y2) || (x4 <= x1 && x4 >= x2 && y3 >= y1 && y3 <= y2) || (x3 <= x1 && x3 >= x2 && y4 >= y1 && y4 <= y2) || (x3 <= x1 && x4 >= x1 && y3 >= y2 && y4 <= y2) || (x3 <= x2 && x4 >= x2 && y3 >= y1 && y4 <= y1)

{-| Esta função verifica a segunda condição da função colisoesParede que é: se o Personagem colide com alguma das Plataformas do Mapa. Isto é conseguido 
através novamente do conceito de HitBox , isto é, da interseção da HitBox dos Personagens com alguma das HitBoxes das Plataformas.

== mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
               [Plataforma,Plataforma,Plataforma,Plataforma],
               [Vazio,     Vazio,     Vazio,     Vazio],
               [Plataforma,Plataforma,Plataforma,Plataforma]]

== Exemplos

>>> colidePlataformaPersonagem (Mapa ((0,0),Este) (0,50) mapaexem) (Personagem (0,0) Jogador (70,-50) Este (12.5,12.5) False False 3 100 (False,0)) 
    = True (O Personagem colide com a HitBox de algum dos Blocos de Plataforma do mapaexem)

>>> colidePlataformaPersonagem (Mapa ((0,0),Este) (0,50) mapaexem) (Personagem (0,0) Jogador (0,-10) Este (12.5,12.5) False False 3 100 (False,0)) 
    = False (O Personagem não colide com os Blocos Plataforma, ou seja está em Blocos Vazio.)
-}

colidePersonagemPlataforma :: Mapa -> Personagem -> Bool 
colidePersonagemPlataforma mapa pers = or (map (colideHitbox (hitbox pers)) (hitboxBlocos Plataforma mapa tBls))

