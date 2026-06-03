module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play
     (InWindow "MAIN" (500,500) (0,0))
     white
     100
     estadoinicial
     draw 
     reageEvento
     reageTempo

data Direcao = Subir | Descer
type Estado = (Direcao, Float, Float)

estadoinicial :: Estado
estadoinicial = (Descer,0,0)

draw :: Estado -> Picture
draw (_,x,y) = Translate x y $ rectangleSolid 50 50

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (d,x,y) = (d,x+10,y)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (d,x,y) = (d,x-10,y)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (d,x,y) = (d,x,y+10)
reageEvento (EventKey (SpecialKey KeyUp) Up _ _) (d,x,y) = (d,x,y-10)
reageEvento _ (d,x,y) = (d,x,y)

reageTempo :: Float -> Estado -> Estado
reageTempo _ (Descer,x,y) = let pYnorm = y-10
                            in if pYnorm < (-225) then 
                                (Subir, x, y)
                               else 
                                (Descer, x, pYnorm)
reageTempo _ (Subir,x,y) = let pYnorm = y+10
                            in if pYnorm > 225 then 
                                (Descer, x, y)
                               else 
                                (Subir, x, pYnorm)