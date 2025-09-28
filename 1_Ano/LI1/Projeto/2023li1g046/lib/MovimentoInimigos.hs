module MovimentoInimigos where

import Tarefa4
import Tarefa3
import LI12324
import Jogador
import Tarefa1
import EncontraPosicaoBlocos

movimentoInimigoAleatorio :: Int -> Mapa -> Personagem -> [Maybe Acao]
movimentoInimigoAleatorio seed mapa inimigo@(Personagem (vx,vy) tipo (x,y) direcao _ escada _ vida _ _) | tipo == MacacoMalvado = [Nothing]
                                                                                                        | mod numeroaleatorio 2 == 0 && estaNumaEscadaBaixo mapa inimigo && not escada && estaNoMeioDaPlataforma mapa inimigo = [Just Subir]
                                                                                                        | mod numeroaleatorio 2 == 0 && estaNumaEscadaCima mapa inimigo && not escada && estaNoMeioDaPlataforma mapa inimigo = [Just Descer]
                                                                                                        | even numeroaleatorio && chegouAoFimDaEscada inimigo mapa && estaNoMeioDaPlataforma mapa inimigo = [Just AndarDireita]
                                                                                                        | odd numeroaleatorio && chegouAoFimDaEscada inimigo mapa && estaNoMeioDaPlataforma mapa inimigo = [Just AndarEsquerda]
                                                                                                        | direcao == Oeste && (colisoesParede mapa (setPosicao (x',y') inimigo) || vaiSairDaPlataforma inimigo mapa) = [Just AndarDireita]
                                                                                                        | direcao == Este && (colisoesParede mapa (setPosicao (x',y') inimigo) || vaiSairDaPlataforma inimigo mapa)  = [Just AndarEsquerda]
                                                                                                        | direcao == Este && not escada = [Just AndarDireita]
                                                                                                        | direcao == Oeste && not escada = [Just AndarEsquerda]
                                                                                                        | escada && vy > 0 && not (chegouAoFimDaEscada inimigo mapa) = [Just Subir]
                                                                                                        | escada && vy < 0 = [Just Descer] 
            where numeroaleatorio = head (geraAleatorios seed 1)
                  (x',y') = (x+vx,y+vy)

vaiSairDaPlataforma :: Personagem -> Mapa -> Bool
vaiSairDaPlataforma pers mapa | any (estaNumBloco limPersonagem) limPlataformas = any (estaNumBloco proximoLimPersonagem) limVazio
                              | otherwise = False 
      where limPlataformas = limitesBlocos Plataforma mapa
            limPersonagem = limitesHitboxPersonagem pers
            limVazio = limitesBlocos Vazio mapa
            (vx,vy) = getVelocidade pers 
            (x,y) = getPosicao pers 
            (x',y') = (x+vx,y+vy)
            proximoLimPersonagem = limitesHitboxPersonagem (setPosicao (x',y') pers )

estaNoMeioDaPlataforma :: Mapa -> Personagem -> Bool 
estaNoMeioDaPlataforma mapa pers = any (checkEstaNoMeioDoBloco (limitesHitboxPersonagem pers)) (limitesBlocos Plataforma mapa)
      where  (((e,f),(g,h)):t) = limitesBlocos Plataforma mapa
             ((a,b),(c,d)) = limitesHitboxPersonagem pers
             checkEstaNoMeioDoBloco ((a,b),(c,d)) ((e,f),(g,h)) = a - 15 == e && b + 15 == f && c == h
