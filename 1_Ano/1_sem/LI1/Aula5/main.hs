module Main where
import Graphics.Gloss

main :: IO ()
main = display displayMode bkgColor pict

displayMode :: Display
displayMode = InWindow "Aula" (500,500) (10,10)

bkgColor :: Color
bkgColor = white

pict :: Picture
pict = Pictures [tile1, Translate 100 0 tile1]

tile1 :: Picture
tile1 = Pictures [quadrado 100,
                 Translate (-50) (-50) (Arc 0 90 50),
                 Translate 50 (-50) (Arc 90 180 50),
                 Translate 50 50 (Arc 180 270 50),
                 Translate (-50) 50 (Arc 270 0 50)]

quadrado :: Float -> Picture
quadrado lado = rectangleWire lado lado

circulo :: Float -> Picture 
circulo raio = ThickCircle raio 10