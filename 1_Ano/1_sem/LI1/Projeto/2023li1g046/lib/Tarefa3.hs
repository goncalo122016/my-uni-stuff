{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use all" #-}
{-# HLINT ignore "Use any" #-}
{-# HLINT ignore "Redundant map" #-}

{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Gonçalo José Vieira de Castro <a107337@alunos.uminho.pt>
              Luis Miguel Jeronimo Felicio <a106913@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Jogador
import Tarefa1
import Functions
import EncontraPosicaoBlocos
import Data.Fixed (mod')
import System.Random

{-| A função movimenta tem como objetivo principal animar todos os personagens, isto é calcular as suas novas posições e respectivas consequências. Algumas 
das condições implementadas na função estão descritas no projeto; em forma esclarecimento e síntese: um inimigo perde uma vida se estiver dentro da hitbox de dano de
um jogador armado (adiquiriu o martelo por um tempo específico) - por jogador armado entende-se um jogador cuja componente aplicaDano esteja True e com tempo 
restante; um inimigo morre quando as suas vidas chegam a 0, nessa altura, deixa de ser representado no mapa; qualquer personagem que não esteja sobre uma 
plataforma deverá cair; o jogador deverá perder uma vida se for atingido por um inimigo; ao recolher um coleccionável, este deverá desaparecer do mapa; no
caso do martelo, este deve armar o jogador durante 10 segundos a contar do momento da sua recolha. No caso de uma moeda, esta deve aumentar a pontuação do 
jogador; um alçapão deverá desaparecer se o jogador o pisar, por outro lado, não sofrerá qualquer alteração se for um inimigo a pisá-lo; personagens não podem 
sair do mapa nem atravessar blocos de plataforma. 
    A função movimentaJogador vai aplicar as condições efetivamente aplicáveis ao Jogador, por exemplo o apanhar dos Colecionáveis, a ação deste Jogador sobre 
os Alçapões, entre várias outras...

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> movimenta 10 0 (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                         [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 3 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 3 100 (False,0))]
                         [(Moeda, (30,-30)), (Martelo, (30,-30))]
                         (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,-10.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,-10.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,-10.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 2, pontos = 100, aplicaDano = (False,0.0)}}

>>> movimentaJogador 10 0 (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                                [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 3 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 3 100 (False,0))]
                                [(Moeda, (30,-30)), (Martelo, (30,-30))]
                                (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Alcapao,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,-10.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,-10.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,-10.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 2, pontos = 100, aplicaDano = (False,0.0)}}
-}

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta seed t jogo@(Jogo mapa ini c jog) = alcapaoDesaparece t $ apanhaColecionaveis $ efeitoGravidade $ tiraInimigosMortos [] $ jogadorSofreDano $ matainimigos $ Jogo mapa (map fazMovimento ini ) c (fazMovimento jog)

movimentaJogador :: Semente -> Tempo -> Jogo -> Jogo
movimentaJogador seed t jogo@(Jogo mapa ini c jo) = alcapaoDesaparece t $ apanhaColecionaveis $ efeitoGravidade $ jogadorSofreDano $ matainimigos $ Jogo mapa ini c (fazMovimento jo)

{-| Esta função tiraInimigosMortos retira do jogo os Inimigos que tenham as vidas a 0, ou seja que tenham sido mortos pelo Jogador.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> tiraInmigosMortos [] (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                               [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 0 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                               [(Moeda, (30,-30)), (Martelo, (30,-30))]
                               (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,-10.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,-10.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 2, pontos = 100, aplicaDano = (False,0.0)}}
    (neste exemplo como o inimigo do tipo Fantasma tem as vidas a 0 a função retira-o do Jogo)

>>> tiraInmigosMortos [] (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                               []
                               [(Moeda, (30,-30)), (Martelo, (30,-30))]
                               (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,-10.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}}
    (neste caso a lista de inimigos é vazia então o Jogo retornado é o mesmo do input)
-}

tiraInimigosMortos :: [Personagem] -> Jogo ->  Jogo 
tiraInimigosMortos acc (Jogo mapa [] c player) = Jogo mapa acc c player
tiraInimigosMortos acc (Jogo mapa (ini:inis) c player) = if getVida ini <= 0 then tiraInimigosMortos acc (Jogo mapa inis c player ) else tiraInimigosMortos (acc ++ [ini]) (Jogo mapa inis c player ) 

{-| Neste caso da função matainimigos temos a lógica prevista no projeto, que se o jogador colidir com um inimigo enquanto estiver armado (ter apanhado o martelo
e ter tempo no momento) estaremos perante a perca de uma vida por parte do inimigo.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> matainimigos (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                       [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 1 100 (False,0)), (Personagem (100,100) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                       [(Moeda, (30,-30)), (Martelo, (30,-30))]
                       (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (True,3)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[VazioJogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 0, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (100.0,100.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (True,3.0)}},Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 0, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (100.0,100.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (True,3.0)}}
      (o inimigo Fantasma passa de uma vida para zero!)
-}

matainimigos :: Jogo -> Jogo
matainimigos j@(Jogo m inimigos c jogador) | d && t > 0 && getDirecao jogador == Este = Jogo m  (map (\(Personagem a b c e f g h v i k) -> Personagem a b c e f g h (v-1) i k) (filter (\(Personagem _ _ (x,y) _ (w,h) _ _ _ _ _) -> colideHitbox (hitboxDano jogador Este) (hitboxmedidas (x,y) (w,h))) inimigos) ++ filter (\(Personagem _ _ (x,y) _ (w,h) _ _ _ _ _) -> not (colideHitbox (hitboxDano jogador Este) (hitboxmedidas (x,y) (w,h)))) inimigos) c jogador
                                           | d && t > 0 && getDirecao jogador == Oeste = Jogo m  (map (\(Personagem a b c e f g h v i k) -> Personagem a b c e f g h (v-1) i k) (filter (\(Personagem _ _ (x,y) _ (w,h) _ _ _ _ _) -> colideHitbox (hitboxDano jogador Oeste) (hitboxmedidas (x,y) (w,h))) inimigos) ++ filter (\(Personagem _ _ (x,y) _ (w,h) _ _ _ _ _) -> not (colideHitbox (hitboxDano jogador Oeste) (hitboxmedidas (x,y) (w,h)))) inimigos) c jogador
                                           | otherwise = j
    where (d,t) = getAplicaDano jogador

{-| As três funções abaixo (efeitoGravidade, naoEstaNoVazio, estaNoVazio) estão relacionadas e todas funcionam para aplicar o efeito da Gravidade aos Personagens,
que como descrito anteriormente e no enunciado do Projeto funciona do seguinte modo: se o Personagem estiver a "flutuar" no mapa a sua velocidade na componente do y
deverá diminuir, para que volte para o "chão"(Plataformas).

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> efeitoGravidade (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                          [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 0 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                          [(Moeda, (30,-30)), (Martelo, (30,-30))]
                          (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,-10.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 0, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,-10.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,-10.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}}
      (neste caso todos os Personagens se encontram num bloco Vazio e assim todas as suas velocidades são alteradas para (vx,-10), como esperado)
-}

efeitoGravidade :: Jogo -> Jogo
efeitoGravidade j@(Jogo m@(Mapa _ _ b) inimigos c jog@(Personagem (x,y) v p d e f g h i k )) = if estaNoVazio m jog then Jogo m (map (\(Personagem (x,y) b c e f g h v i k) -> Personagem (x,-10) b c e f g h v i k) (filter (estaNoVazio m) inimigos) ++ filter (naoEstaNoVazio m) inimigos) c (Personagem (x,-10) v p d e f g h i k)
                                                                                                                    else Jogo m (map (\(Personagem (x,y) b c e f g h v i k) -> Personagem (x,-10) b c e f g h v i k) (filter (estaNoVazio m) inimigos) ++ filter (naoEstaNoVazio m) inimigos) c jog 

naoEstaNoVazio :: Mapa -> Personagem -> Bool
naoEstaNoVazio m pers  = elem True (map (estaNumBloco limJogador) limBlocos) || getEmEscada pers
      where limJogador = limitesHitboxPersonagem pers
            limBlocos = limitesBlocos Plataforma m ++ limitesBlocos Alcapao m

estaNoVazio :: Mapa -> Personagem -> Bool
estaNoVazio m pers  = notElem True (map (estaNumBloco limJogador) limBlocos) && not (getEmEscada pers)
      where limJogador = limitesHitboxPersonagem pers
            limBlocos = limitesBlocos Plataforma m ++ limitesBlocos Alcapao m

{-| Análogamente à matainimigos, a função jogadorSofreDano é o caso em que um jogador colide com um inimigo mas desta vez o Jogador não está armado e assim, desta
vez é o Jogador que perde uma vida.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> jogadorSofreDano (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                           [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 1 100 (False,0)), (Personagem (100,100) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                           [(Moeda, (30,-30)), (Martelo, (30,-30))]
                           (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (100.0,100.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Moeda,(30.0,-30.0)),(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (100.0,-10.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 2, pontos = 100, aplicaDano = (False,0.0)}}
      (desta vez o Jogador, em colisão com o Fantasma, perde uma via, passando para 2 vidas! (em vez de 3 iniciais).)
-}

jogadorSofreDano :: Jogo -> Jogo
jogadorSofreDano j@(Jogo m inimigos c jogador@(Personagem a b n e f g h v i k)) | or (map (colideHitbox (hitbox jogador)) (map hitbox inimigos)) = Jogo m inimigos c (Personagem a b n e f g h (v-1) i k)
                                                                                | otherwise = j

{-| A função apanhaColecionaveis (acompanhada pela colideComColecionaveis) trata do facto de o jogador poder apanhar, durante o Jogo, moedas ou o martelo. Se este
Jogador apanhar um dos colecionáveis este deverá desaparecer do Jogo. Se for uma moeda a sua pontuação aumenta por 100 pontos, se for um martelo o jogador fica "armado"
durante 10 segundos.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> apanhaColecionaveis (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                           [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 1 100 (False,0)), (Personagem (100,100) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                           [(Moeda, (0,0)), (Martelo, (30,-30))]
                           (Personagem (0,0) Jogador (0,0) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (100.0,100.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(30.0,-30.0))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (0.0,0.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 200, aplicaDano = (False,0.0)}}
      (o Jogador, em colisão com uma Moeda, passa de 100 pontos para 200!)

>>> apanhaColecionaveis (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                           [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 1 100 (False,0)), (Personagem (100,100) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                           [(Moeda, (30,-30)), (Martelo, (0,0))]
                           (Personagem (0,0) Jogador (0,0) Este (12.5,12.5) False False 3 100 (False,0)))
    = Jogo {mapa = Mapa ((0.0,0.0),Este) (0.0,50.0) [[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (100.0,-10.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)},Personagem {velocidade = (100.0,100.0), tipo = MacacoMalvado, posicao = (10.0,0.0), direcao = Oeste, tamanho = (40.0,40.0), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(0.0,0.0))], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (0.0,0.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 200, aplicaDano = (True,10.0)}}
      (o Jogador, em colisão com um Martelo desta vez a estar armado - aplicaDano = (True,10))
-}

apanhaColecionaveis :: Jogo -> Jogo 
apanhaColecionaveis j@(Jogo m inimigos c jogador) = Jogo m inimigos colRestantes jogComCol
      where (colRestantes,jogComCol) = colideComColecionaveis [] c jogador

colideComColecionaveis :: [(Colecionavel,Posicao)] -> [(Colecionavel, Posicao)] -> Personagem -> ([(Colecionavel, Posicao)],Personagem)
colideComColecionaveis acc [] pers = (acc,pers)
colideComColecionaveis acc col@(c:cs) pers | not (any (colideHitbox (hitbox pers)) (map hitboxcolecionaveis col)) = (acc++col,pers)
                                           | colideHitbox (hitbox pers) (hitboxcolecionaveis c) && fst c == Martelo = (acc++cs,setAplicaDano (True,10) pers)
                                           | colideHitbox (hitbox pers) (hitboxcolecionaveis c) && fst c == Moeda = (acc++cs, setPontos (pontos +100) pers)
                                           | not (colideHitbox (hitbox pers) (hitboxcolecionaveis c)) = colideComColecionaveis (acc ++ [c]) cs pers 
      where pontos = getPontos pers 

{-| Este grupo de funções concretiza algumas restrições de posições irregulares ou regulares (dependendo do caso) de Personagens (Jogador e Inimigos) e de Colecionáveis.

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

>>> posicaoDePersonagensValido (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                                     [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 1 100 (False,0)), (Personagem (100,100) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                                     [(Moeda, (30,-30)), (Martelo, (0,0))]
                                     (Personagem (0,0) Jogador (0,0) Este (12.5,12.5) False False 3 100 (False,0)))
    = False (o Inimigo Fantasma, por exempo, está fora do mapa, neste caso)

-}

posicaoDePersonagensValido :: Jogo -> Bool
posicaoDePersonagensValido (Jogo m inimigos c jogador) = (notElem True (map (colisoesParede m) inimigos) || not (colisoesParede m jogador)) && not (or (map (jogadorEstaDentroBloco limJogador) limBlocos)) && not (or (map (personagemEstaDentroBloco limInimigos) limBlocos))
      where jogadorEstaDentroBloco ((a,b),(c,d)) ((e,f),(g,h)) = ((a <= e && b > e) || (a < f && b >= f) || (a > e && b < f)) && ((c <= h && c >= g) || (d <= h && d >= g) || (d > h && c < h)) 
            limBlocos = limitesBlocos Plataforma m ++ limitesBlocos Alcapao m
            limInimigos = map limitesHitboxPersonagem inimigos
            limJogador = limitesHitboxPersonagem jogador 

posicaoColecionavelValida :: [(Colecionavel,Posicao)] -> Mapa -> Bool
posicaoColecionavelValida (c@(_,(x,y)):t) m  = (mod' x 5 == 0) && (mod' y 5 == 0) && or (map (colideHitbox (hitboxcolecionaveis c)) (hitboxBlocos Vazio m tBls))

personagemEstaDentroBloco :: [((Double,Double),(Double,Double))] -> ((Double,Double),(Double,Double)) -> Bool
personagemEstaDentroBloco [] _ = False 
personagemEstaDentroBloco (((a,b),(c,d)):t) limBls@((e,f),(g,h)) = ((a <= e && b > e) || ( a < f &&  b >= f) || (a > e && b < f)) && ((abs c > abs h && abs c <= abs g) || (abs d >= abs h && abs d <= abs g) || (abs d < abs h && abs c > abs h) || (abs d <= abs h && abs c >= abs g) || ( c > h && c <= g)) || personagemEstaDentroBloco t limBls
            
alcapaoDesaparece :: Tempo -> Jogo -> Jogo
alcapaoDesaparece t j@(Jogo m@(Mapa _ _ bls) ini col jogador) | or ativouAlcapao && getDirecao jogador == Este && estaNumAlcapaoDireita jogador m = Jogo (trocaNEsimoBloco Alcapao posicaoAlcapao Vazio m) ini col jogador
                                                              | or ativouAlcapao && getDirecao jogador == Oeste && estaNumAlcapaoEsquerda jogador m = Jogo (trocaNEsimoBloco Alcapao posicaoAlcapao Vazio m) ini col jogador
                                                              | otherwise = j 
      where ativouAlcapao = alcapaoAtivado j 
            posicaoAlcapao = posicaoBool 1 True ativouAlcapao 

 
alcapaoAtivado :: Jogo -> [Bool]
alcapaoAtivado (Jogo m _ _ jogador) = map (estaNumBloco limJogador) limAlcapoes 
      where limAlcapoes = limitesBlocos Alcapao m
            limJogador = limitesHitboxPersonagem jogador 

posicaoBool :: Int -> Bool -> [Bool] -> Int 
posicaoBool n b (h:t) = if b == h then n else posicaoBool (n+1) b t  

estaNumBloco ((a,b),(c,d)) ((e,f),(g,h)) =((a <= e && b > e) || (a < f && b >= f) || (a > e && b < f)) && c == h 

estaNumaEscadaCima :: Mapa -> Personagem -> Bool 
estaNumaEscadaCima m p = or (map (naEscadaCima (limitesHitboxPersonagem p)) (limitesBlocos Escada m)) 
  where naEscadaCima ((x1,x2),(y1,y2)) ((x3,x4),(y3,y4)) = x1 >= x3 && x2 <= x4 && y1 == snd tBls + y4

estaNumaEscadaBaixo :: Mapa -> Personagem -> Bool
estaNumaEscadaBaixo m p = or (map (naEscadaBaixo (limitesHitboxPersonagem p)) (limitesBlocos Escada m))
      where naEscadaBaixo ((x1,x2),(y1,y2)) ((x3,x4),(y3,y4)) = x1 >= x3 && x2 <= x4 && y1 == y3

velocidadeSobeEscada = (0.0,3.0)

velocidadeDesceEscada = (0.0,-3.0)

chegouAoFimDaEscada :: Personagem -> Mapa -> Bool 
chegouAoFimDaEscada pers mapa | getEmEscada pers && (getVelocidade pers == velocidadeSobeEscada || getVelocidade pers == (0,0)) && estaNumaEscadaCima mapa pers = True 
                              | getEmEscada pers && (getVelocidade pers == velocidadeDesceEscada || getVelocidade pers == (0,0)) && estaNumaEscadaBaixo mapa pers = True
                              | otherwise = False

{-| A função fazMovimento efeitiva um movimento de uma personagem, isto é, tendo a Personagem uma Velocidade específica, esta função adiciona o vetor Velocidade
à posição da própria Personagem resultando a posição seguinte da mesma.

== Exemplo

>>> fazMovimento (Personagem (10,-10) Jogador (3,25) Este (12.5,12.5) False False 3 100 (False,0)) 
    = Personagem {velocidade = (10.0,-10.0), tipo = Jogador, posicao = (13.0,15.0), direcao = Este, tamanho = (12.5,12.5), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}
-}

fazMovimento :: Personagem -> Personagem 
fazMovimento (Personagem (vx,vy) t (x,y) d ta e r v p ad) = Personagem (vx,vy) t (x+vx,y+vy) d ta e r v p ad

{-| Este grupo de funções realiza a lógica por detrás dos Alçapões envolvendo todo o seu funcionamento, como o facto de estes desaparecerem momentos após o 
Jogador pisar neles, mas no entanto os Inimigos não possuem qualquer efeito sobre estes, por exemplo...

== Exemplo

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Alcapao,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Alcapao,Plataforma,Plataforma]]

>>> alcapaoAtivado (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                         [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 1 100 (False,0)), (Personagem (100,100) MacacoMalvado (10,0) Oeste (40,40) False False 1 100 (False,0))]
                         [(Moeda, (30,-30)), (Martelo, (0,0))]
                         (Personagem (0,0) Jogador (0,0) Este (12.5,12.5) False False 3 100 (False,0)))
    = [False,False] (como ambos os Alçapões do Mapa não forma ativados ambos devolvem o estado False)
-} 

estaNumAlcapaoDireita :: Personagem -> Mapa -> Bool
estaNumAlcapaoDireita pers mapa = any (estaASairDoAlcapaoDireita limitesPersonagem) limitesAlcapoes
      where limitesPersonagem@((x1,x2),(y1,y2)) = limitesHitboxPersonagem pers 
            limitesAlcapoes@(((x3,x4),(y3,y4)):t) = limitesBlocos Alcapao mapa
            estaASairDoAlcapaoDireita ((x1,x2),(y1,y2)) ((x3,x4),(y3,y4)) = x1 <= x4 && x2 > x4 && y4 == y1

estaNumAlcapaoEsquerda :: Personagem -> Mapa -> Bool
estaNumAlcapaoEsquerda pers mapa = any (estaASairDoAlcapaoEsquerda limitesPersonagem) limitesAlcapoes
      where limitesPersonagem@((x1,x2),(y1,y2)) = limitesHitboxPersonagem pers 
            limitesAlcapoes@(((x3,x4),(y3,y4)):t) = limitesBlocos Alcapao mapa
            estaASairDoAlcapaoEsquerda ((x1,x2),(y1,y2)) ((x3,x4),(y3,y4)) = x1 < x3 && x2 >= x3 && y4 == y1