module Main where

import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Graphics.Gloss
import LI12324
import Graphics.Gloss.Interface.Pure.Game
import EncontraPosicaoBlocos

type Estado = ((Float,Float),[Picture],Mapa)

estadoinicial :: [Picture] -> Estado
estadoinicial pics = ((-725,525), pics, mapa2)

desenhamapa :: Estado -> Picture
desenhamapa e = Pictures (charParaPeca e)

charParaPeca :: Estado -> [Picture]
charParaPeca (_ ,pic,Mapa _ _ []) = []
charParaPeca ((x,y),pic,Mapa a c ([]:t)) = charParaPeca ((x-fromIntegral (length (head t))*50,y-50),pic,Mapa a c t)
charParaPeca ((x,y),pic,Mapa a c ((b:bs):t)) |b == Plataforma = Translate x y  (pic !! 1) : charParaPeca ((x+50,y),pic,Mapa a c (bs:t))
                                             |b == Escada = Translate x y  (pic !! 2) : charParaPeca ((x+50,y),pic,Mapa a c (bs:t))
                                             |b == Alcapao = Translate x y (head pic) : charParaPeca ((x+50,y),pic,Mapa a c (bs:t))
                                             |otherwise = Translate x y (pic !! 3) : charParaPeca ((x+50,y),pic,Mapa a c (bs:t))




desenhaestado :: Estado -> Picture
desenhaestado = desenhamapa


reageEvento :: Event -> Estado -> Estado
reageEvento _ e = e

reageTempo :: Float -> Estado -> Estado
reageTempo _ e = e

fr :: Int
fr = 50

dm :: Display
dm = FullScreen
 

main :: IO ()
main = do
    alcapao <- loadBMP "images/alcapao.bmp"
    plataforma <- loadBMP "images/plataforma.bmp" 
    escada <- loadBMP "images/escada.bmp"
    vazio <- loadBMP "images/vazio.bmp" 
    play dm
            black
            fr
            (estadoinicial [alcapao,plataforma,escada,vazio])
            desenhaestado
            reageEvento
            reageTempo

