{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
module Controlos where

import Tarefa4 
import LI12324
import Tarefa3 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Konkey_Donk 

movimentoFluido :: Event -> [Key] -> [Key]
movimentoFluido (EventKey key Up _ _) = delete key
movimentoFluido (EventKey key Down _ _) = (:) key 
movimentoFluido _ = id

--acaoJogador :: Key -> Estado -> Estado
--acaoJogador _ (p,ima,aj,ai,jogo) = (p,ima,Just Parar,ai,jogo)
--acaoJogador (Char 'w') (p,ima,aj,ai,jogo) = (p,ima,Just Subir,ai,jogo) 
--acaoJogador (Char 's') (p,ima,aj,ai,jogo) = (p,ima,Just Descer,ai,jogo)
--acaoJogador (Char 'd') (p,ima,aj,ai,jogo) = (p,ima,Just AndarDireita,ai,jogo)
--acaoJogador (Char 'a') (p,ima,aj,ai,jogo) = (p,ima,Just AndarEsquerda,ai,jogo)
--acaoJogador (SpecialKey KeySpace) (p,ima,aj,ai,jogo) = (p,ima,Just Saltar,ai,jogo)
 
