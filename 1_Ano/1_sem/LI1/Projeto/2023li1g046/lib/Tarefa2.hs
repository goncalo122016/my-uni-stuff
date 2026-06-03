{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Use notElem" #-}
{-# HLINT ignore "Use notElem" #-}
{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Gonçalo José Vieira de Castro <a107337@alunos.uminho.pt>
              Luis Miguel Jeronimo Felicio <a106913@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import Decuple
import LI12324
import Jogador
import Tarefa1
import Functions
import EncontraPosicaoBlocos

{-| A função valida efetivamente valida um Jogo segundo as restrições impostas no Enunciado do Projeto que serão descritos através das funções auxiliares.
De forma sucinta as restrições são: 1- O mapa tem de ter “chão”. ou seja o fundo do mapa tem de ser constituído apenas por Plataformas; 2- Todos os inimigos 
têm a propriedade ressalta a True, enquanto que o jogador a tem a False; 3- A posição inicial de um jogador não pode coincidir com a posição inicial de um 
outro personagem, ainda assim as posições iniciais de inimigos podem colidir entre estes; 4- Número mínimo de inimigos é 2; 5- Inimigos Fantasma têm exactamente
1 vida; 6- Escadas não podem começar/terminar em alçapões, e pelo menos uma das suas extremidades tem que ser do tipo Plataforma; 7- Alçapões não podem ser menos
largos que o jogador; 8- Não podem existir personagens nem coleccionáveis “dentro” de plataformas ou alçapões, isto é o bloco (na matriz do mapa) correspendente
à posição de um personagem ou objecto tem que ser Vazio.

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

== Exemplos

>>> valida (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
            [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 3 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 3 100 (False,0))]
            [(Moeda, (30,-30)), (Martelo, (30,-30))]
            (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0))
    = False (Deu False pois: A propriedade dos inimigos está False, o inimigo Fantasma tem mais que uma vida (tem 3) e a posição do Jogador coincide com a do Fantasma.)

>>> valida (Jogo (Mapa ((0,0),Este) (0,50) mapaexemp) 
            [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False True 1 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False True 3 100 (False,0))] 
            [(Moeda, (30,-30)), (Martelo, (30,-30))] 
            (Personagem (0,0) Jogador (10,30) Este (12.5,12.5) False False 3 100 (False,0)))
    = True (Corrigido tudo o que estava "mal" no Exemplo anterior)
-}

valida :: Jogo -> Bool
valida jogo@(Jogo m@(Mapa _ _ bloc) inim colec jog) = temChao m && (all estadoRessalta inim && not (estadoRessalta jog)) && all (/= getPosicao jog) (map getPosicao inim) && length inim >=2 && all inimigosFantasma1 inim && construcaoDeEscadas m && tamanhoAlcapao m jog && personagensColecionaveisValidos jogo

{-| Esta função temChao verifica a 1ª Condição, ou seja verifica se a última linha da Matriz de [[Blocos]] é toda constituída por apenas Plataformas.

== Exemplos

>>> temChao (Mapa ((0,0),Este) (0,50) [[Vazio,     Vazio,     Vazio],
                                       [Plataforma,Escada,    Alcapao],
                                       [Plataforma,Plataforma,Plataforma]])
    = True

>>> temChao (Mapa ((0,0),Este) (0,50) [[Vazio,     Vazio, Vazio],
                                       [Plataforma,Escada,Alcapao],
                                       [Plataforma,Vazio, Vazio]])
    = False
-}

temChao :: Mapa -> Bool
temChao (Mapa _ _ bloc) = all (== Plataforma) (last bloc)

{-| A função estadoRessalta verifica a 2ª Condição, isto é extrai da Personagem a propriedade Ressalta com auxílio de uma função definida no ficheiro Jogador.hs.

== Exemplos

>>> estadoRessalta (Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False True 1 100 (False,0)) = True

>>> estadoRessalta (Personagem (0,0) Jogador (10,30) Este (40,70) False False 3 100 (False,0)) = False
-}

estadoRessalta :: Personagem -> Bool
estadoRessalta = getRessalta

{-| A função inimigosFantasma1 verifica a 5ª Condição, isto é extrai da Personagem (neste caso só dos Fantasmas) o número de Vidas com auxílio de uma função definida 
novamente ficheiro Jogador.hs e compara, verificando se é só uma.

== Exemplos

>>> inimigosFantasma1 (Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False True 1 100 (False,0)) = True

>>> inimigosFantasma1 (Personagem (0,0) Fantasma (10,30) Este (40,40) False False 3 100 (False,0)) = False
-}

inimigosFantasma1 :: Personagem -> Bool
inimigosFantasma1 p | getTipo p == Fantasma = getVida p == 1
                    | otherwise = True

{-| Este grupo de funções construcaoDeEscadas e checkContrucaoEscadas verificam a 6ª Condição, ou seja, na presença de Blocos do tipo Escada numa Matriz 
[[Blocos]] verifica se esta está bem construída em relação aos pontos referidos acima na função valida. Em suma uma Escada está bem construída se: não 
começa/termina em alçapões, e pelo menos uma das suas extremidades é do tipo Plataforma.

== Exemplos

>>> construcaoDeEscadas (Mapa ((0,0),Este) (0,50) [[Vazio,     Vazio,     Vazio],
                                                   [Plataforma,Escada,    Alcapao],
                                                   [Plataforma,Alcapao,Plataforma]])
    = False (Ambas as condições falham!)

>>> construcaoDeEscadas (Mapa ((0,0),Este) (0,50) [[Vazio,     Vazio,     Vazio],
                                                   [Plataforma,Escada,    Alcapao],
                                                   [Plataforma,Plataforma,Plataforma]])
    = True
-}

construcaoDeEscadas :: Mapa -> Bool
construcaoDeEscadas m = all (checkContrucaoEscadas m) posEsc
    where posEsc = encontraPosicaoBlocoMatriz Escada (0,0) m ++ encontraPosicaoBlocoMatriz EscadaIncompleta (0,0) m 

checkContrucaoEscadas :: Mapa -> Posicao -> Bool -- dada a posição de uma escada verifca se ela está bem construida
checkContrucaoEscadas m (i,j)    | encontraBlocoMatriz Alcapao (i,j-1) m || encontraBlocoMatriz Alcapao (i,j+1) m = False
                                 | encontraBlocoMatriz Plataforma (i,j+1) m || encontraBlocoMatriz Plataforma (i,j-1) m = True
                                 | otherwise = True

{-| A função tamanhoAlcapao verifica a 7ª Condição que é relativa ao tamanho do Alçapão ter de ser maior que a largura do Jogador para que este possa cair 
através dele.

== Exemplos

>>> tamanhoAlcapao (Mapa ((0,0),Este) (0,50) [[Vazio,     Vazio,     Vazio],
                                              [Plataforma,Escada,    Alcapao],
                                              [Plataforma,Alcapao,Plataforma]])
                        (Personagem (0,0) Jogador (10,30) Este (20,70) False False 3 100 (False,0))
    = True

>>> tamanhoAlcapao (Mapa ((0,0),Este) (0,50) [[Vazio,     Vazio,     Vazio],
                                              [Plataforma,Escada,    Alcapao],
                                              [Plataforma,Plataforma,Plataforma]])
                        (Personagem (0,0) Jogador (10,30) Este (50,70) False False 3 100 (False,0))
    = False 
-}

tamanhoAlcapao :: Mapa -> Personagem -> Bool
tamanhoAlcapao m j    | hitboxBlocos Alcapao m tBls /= [] = c < tAlc
                      | otherwise = True
    where (c,_) = getTamanho j
          hitboxAlc = head (hitboxBlocos Alcapao m tBls)
          tAlc = abs ((fst . fst $ hitboxAlc) - (fst . snd $ hitboxAlc))

{-| Estas 4 funções abaixo funcionam todas para validar a condição 8: não podem existir personagens(inimigosValidos e jogadorvalido) nem 
coleccionáveis(colecionaveisValidos) “dentro” de plataformas ou alçapões, isto é o bloco (na matriz do mapa) correspendente à posição de um personagem 
ou objeto tem que ser Vazio. E tudo culmina na última função personagensColecionaveisValidos que junta todas as condições na restrição final pretendida.

==  mapaexem = [[Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma],
                [Vazio,     Vazio,     Vazio,     Vazio],
                [Plataforma,Plataforma,Plataforma,Plataforma]]

== Exemplos

>>> personagensColecionaveisValidos (Jogo (Mapa ((0,0),Este) (0,50) mapaexem) 
                                     [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False False 3 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False False 3 100 (False,0))]
                                     [(Moeda, (30,-30)), (Martelo, (30,-30))]
                                     (Personagem (0,0) Jogador (100,-10) Este (12.5,12.5) False False 3 100 (False,0)))
    = False (A posição do Jogador coincide com a do Fantasma)

>>> personagensColecionaveisValidos (Jogo (Mapa ((0,0),Este) (0,50) mapaexemp) 
                                     [(Personagem (0,0) Fantasma (100,-10) Oeste (40,40) False True 1 100 (False,0)), (Personagem (0,0) MacacoMalvado (10,0) Oeste (40,40) False True 3 100 (False,0))] 
                                     [(Moeda, (30,-30)), (Martelo, (30,-30))] 
                                     (Personagem (0,0) Jogador (10,30) Este (12.5,12.5) False False 3 100 (False,0)))
    = True
-}

personagensColecionaveisValidos :: Jogo -> Bool
personagensColecionaveisValidos jogo = inimigosValidos jogo && colecionaveisValidos jogo && jogadorvalido jogo

inimigosValidos :: Jogo -> Bool
inimigosValidos (Jogo _ [] c j) = True 
inimigosValidos (Jogo m (i:is) c j) = not (any (colideHitbox (hitbox i)) (hitboxBlocos Plataforma m tBls)) && not (any (colideHitbox (hitbox i)) (hitboxBlocos Alcapao m tBls)) && inimigosValidos (Jogo m is c j) -- O tBls é o tamnaho do bloco que depois podem ser mudadas quando tivermos as dimensões que queremos usar no jogo.

colecionaveisValidos :: Jogo -> Bool
colecionaveisValidos (Jogo _ _ [] j) = True
colecionaveisValidos (Jogo m i (c:cs) j) = not (any (colideHitbox (hitboxcolecionaveis c)) (hitboxBlocos Plataforma m tBls)) && not (any (colideHitbox (hitboxcolecionaveis c)) (hitboxBlocos Alcapao m tBls)) && not (any (colideHitbox (hitboxcolecionaveis c)) (hitboxBlocos Escada m tBls)) && colecionaveisValidos (Jogo m i cs j)

jogadorvalido :: Jogo -> Bool
jogadorvalido (Jogo m i c jogador) = not (any (colideHitbox (hitbox jogador)) (hitboxBlocos Plataforma m tBls)) && not (any (colideHitbox (hitbox jogador)) (hitboxBlocos Alcapao m tBls)) && not (any (colideHitbox (hitbox jogador)) (map hitbox i))
