{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Gonçalo José Vieira de Castro <a107337@alunos.uminho.pt>
              Luis Miguel Jeronimo Felicio <a106913@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Tarefa1
import Data.Maybe
import Decuple
import Jogador
import EncontraPosicaoBlocos 
import LI12324
import Tarefa3
import Niveis

jog = Personagem (0,0) Jogador (3,3) Este (12.5,12.5) True False 3 100 (False,0.0)

{-| A função atualiza é projetada para associar ações: ao Jogador segundo os Eventos que forem registados pelo teclado (no módulo Konkey_Donk) e no caso dos 
inimigos as suas ações são provenientes de uma outra função que calcula o seu movimento aleatório já que não são controlados pelo utilizador. A função atualiza
entrega  e aplica essas alterações no estado das Personagens: o primeiro argumento [Maybe Acao] refere-se às ações que serão aplicadas nos inimigos e o parâmetro
Maybe Acao aplica-se ao Jogador.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> atualiza [Just Subir, Just Descer]
             (Just Parar)
             (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                   [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 3 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 3 100 (False,0))]
                   [(Moeda, (30,-30)), (Martelo, (30,-30))]
                   (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}}
-}

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acinim acjog (Jogo mapa inim colec jog) = Jogo mapa (zipWith (aplicaAcao mapa) acinim inim) colec (aplicaAcao mapa acjog jog) 

{-| A função aplicaAcao funciona como auxiliar à função anterior (atualiza) de modo que aplica uma dada Maybe Acao a uma determinada Personagem.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> aplicaAcao (Mapa ((0,0),Este) (0,50) mapaexem) (Just AndarDireita) (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0))
    = Personagem {velocidade = (12.5,0.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}
      (a velocidade do Personagem altera-se para um valor positivo na componente x, assim quando o movimento lhe for aplicado este andará para a direita)
-}

aplicaAcao :: Mapa -> Maybe Acao -> Personagem -> Personagem
aplicaAcao mapa (Just Subir) p | getEmEscada p && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) && not (estaNumaEscadaBaixo mapa p) = setVelocidade (0,0) p
                               | not (getRessalta p) && not (getEmEscada p) && colidePersonagemPlataforma mapa (setPosicao (x'',y'') p) && estaNoVazio mapa p = setVelocidade (0,- menorDistanciaAPlataforma mapa p) p
                               | not (getRessalta p) && not (getEmEscada p) && estaNumaEscadaBaixo mapa p = setEmEscada True $ setVelocidade (0,10) p 
                               | getRessalta p && not (getEmEscada p) && estaNumaEscadaBaixo mapa p = setEmEscada True $ setVelocidade (0,5) p 
                               | getRessalta p && getEmEscada p = setVelocidade (0,5) p
                               | not (getRessalta p) && getEmEscada p = setVelocidade (0,10) p
                               | otherwise = aplicaAcao mapa Nothing p 
    where (vx,vy) = getVelocidade p
          (x'',y'') = (fst(getPosicao p) +vx , snd (getPosicao p)+vy)
aplicaAcao mapa (Just Descer) p | getEmEscada p && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) && not (estaNumaEscadaCima mapa p) = setVelocidade (0,0) p
                                | not (getRessalta p) && not (getEmEscada p) && colidePersonagemPlataforma mapa (setPosicao (x'',y'') p) && estaNoVazio mapa p = setVelocidade (0,- menorDistanciaAPlataforma mapa p) p
                                | not (getRessalta p) && not (getEmEscada p) && estaNumaEscadaCima mapa p = setEmEscada True $ setVelocidade (0,-10) p
                                | getRessalta p && not (getEmEscada p) && estaNumaEscadaCima mapa p = setEmEscada True $ setVelocidade (0,-5) p  
                                | not (getRessalta p) && getEmEscada p = setVelocidade (0,-10) p  
                                | getRessalta p && getEmEscada p = setVelocidade (0,-5) p
                                | otherwise = aplicaAcao mapa Nothing p 
    where (vx,vy) = getVelocidade p
          (x',y') = (fst(getPosicao p) + 5 , snd (getPosicao p) + vy)
          (x'',y'') = (fst(getPosicao p) +vx , snd (getPosicao p)+ vy )
aplicaAcao mapa (Just AndarDireita) p | not (getRessalta p) && getEmEscada p && chegouAoFimDaEscada p mapa && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) = setDirecao Este $ setEmEscada False $ setVelocidade (10,0) p
                                      | not (getRessalta p) && not (getEmEscada p) && colidePersonagemPlataforma mapa (setPosicao (x''',y''') p) && estaNoVazio mapa p = setVelocidade (0,- menorDistanciaAPlataforma mapa p) p
                                      | getRessalta p && getEmEscada p && chegouAoFimDaEscada p mapa && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) = setEmEscada False $ setVelocidade (5,0) p
                                      | not (getRessalta p) && estaNoVazio mapa p && any (personagemEstaDentroBloco [limitesHitboxPersonagem (setPosicao (x''',y''') p)]) (limitesBlocos Plataforma mapa ++ limitesBlocos Alcapao mapa) = setVelocidade (0,- menorDistanciaAPlataforma mapa p) p 
                                      | not (getRessalta p) && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) && not (getEmEscada p) && estaNoVazio mapa (setPosicao (xcair,y) p) = setVelocidade (vx,-10) p
                                      | not (getEmEscada p) && not (getRessalta p) && not (colisoesParede mapa (setPosicao (x'',y'') p)) = setVelocidade (10,0) $ setDirecao Este p
                                      | not (getEmEscada p) && getRessalta p = if colisoesParede mapa (setPosicao (x',y') p) || estaNoVazio mapa (setPosicao (x,y) p) then setDirecao Oeste $ setVelocidade (-5,vy) p  else setDirecao Este $ setVelocidade (5,vy) p
                                      | otherwise = aplicaAcao mapa (Just Parar) p 
    where (vx,vy) = getVelocidade p
          (x,y) = getPosicao p 
          (x',y') = (fst(getPosicao p) + 5 , snd (getPosicao p) + vy) --Próxima posição
          (x'',y'') = (fst(getPosicao p) + 10 , snd (getPosicao p)+vy)
          (x''',y''') = (fst(getPosicao p) +10 , snd (getPosicao p)+vy)
          (xcair,_) = (fst(getPosicao p) +5 ,y)
aplicaAcao mapa (Just AndarEsquerda) p | not (getRessalta p) && getEmEscada p && chegouAoFimDaEscada p mapa && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) = setDirecao Oeste $ setEmEscada False $ setVelocidade (-10,0) p
                                       | not (getRessalta p) && not (getEmEscada p) && colidePersonagemPlataforma mapa (setPosicao (x''',y''') p) && estaNoVazio mapa p = setVelocidade (0,- menorDistanciaAPlataforma mapa p) p
                                       | getRessalta p && getEmEscada p && chegouAoFimDaEscada p mapa && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) = setEmEscada False $ setVelocidade (-5,0) p 
                                       | not (getRessalta p) && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) && not (getEmEscada p) && estaNoVazio mapa (setPosicao (xcair,y) p) = setVelocidade (vx,-10) p                          
                                       | not (getEmEscada p) && not (getRessalta p) && not (colisoesParede mapa (setPosicao (x'',y'') p)) = setVelocidade (-10,0) $ setDirecao Oeste p  
                                       | not (getEmEscada p) && getRessalta p = if colisoesParede mapa (setPosicao (x',y') p) || estaNoVazio mapa (setPosicao (x',y') p) then setDirecao Este $ setVelocidade (5,vy) p  else setDirecao Oeste $ setVelocidade (-5,vy) p 
                                       | otherwise = aplicaAcao mapa (Just Parar) p 
    where (vx,vy) = getVelocidade p 
          (x,y) = getPosicao p 
          (x',y') = (fst(getPosicao p) -5 , snd (getPosicao p))  --Próxima posição
          (x'',y'') = (fst(getPosicao p) -10 , snd (getPosicao p) +vy)
          (x''',y''') = (fst(getPosicao p) -10 , snd (getPosicao p)+vy)
          (xcair,_) = (fst(getPosicao p) -5 ,y)
aplicaAcao mapa (Just Saltar) p | not (estaNoVazio mapa p) && not (getEmEscada p) = setVelocidade (vx,80) p
                                | otherwise = aplicaAcao mapa Nothing p 
    where (vx,vy) = getVelocidade p
aplicaAcao mapa (Just Parar) p | estaNoVazio mapa p = setVelocidade (0,vy) p
                               | otherwise = setVelocidade (0,0) p
    where (_,vy) = getVelocidade p
aplicaAcao mapa Nothing p | (not (estaNumaEscadaBaixo mapa p) || not (estaNumaEscadaCima mapa p)) && any (estaNumBloco (limitesHitboxPersonagem p)) (limitesBlocos Plataforma mapa) = setVelocidade (0,0) p 
                          | not (getRessalta p) && not (getEmEscada p) && colidePersonagemPlataforma mapa (setPosicao (x'',y'') p) && estaNoVazio mapa p = setVelocidade (vx,- menorDistanciaAPlataforma mapa p) p
                          | estaNoVazio mapa p = setVelocidade (0,vy) p 
                          | otherwise = setVelocidade (0,0) p
    where (vx,vy) = getVelocidade p 
          (x'',y'') = (fst(getPosicao p) +vx , snd (getPosicao p) -10)

menorDistanciaAPlataforma :: Mapa -> Personagem -> Double
menorDistanciaAPlataforma mapa pers = minimum (map (calculaADistancia (limitesHitboxPersonagem pers)) (limitesBlocos Plataforma mapa))  
    where calculaADistancia ((a,b),(c,d)) ((e,f),(g,h)) = abs (c - h)