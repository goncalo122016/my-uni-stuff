module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap 

main :: IO ()
main = do
     p1 <- loadBMP "out.bmp"
     play (InWindow "MAIN" (500,500) (0,0))
        white
        35
        (estadoinicial p1)
        draw 
        reageEvento 
        reageTempo

type Direcao = (Float, Float)
type Estado = (Picture, Direcao, Float, Float)

estadoinicial :: Picture -> (Picture, (Float,Float), Float, Float)
estadoinicial p1 = (p1,(3,-5),0,0)

draw :: Estado -> Picture
draw (p1,_,x,y) = Translate x y (scale 0.030 0.030 p1)

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (p1,(dx,dy),x,y) = (p1,(dx,dy),x,y+5)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (p1,(dx,dy),x,y) = (p1,(dx,dy),x,y-5)
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) (p1,(dx,dy),x,y) = (p1,(dx,dy),x-5,y)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (p1,(dx,dy),x,y) = (p1,(dx,dy),x+5,y)
reageEvento _ (p1,d,x,y) = (p1,d,x,y)

reageTempo :: Float -> Estado -> Estado
reageTempo _ (p1,(dx,dy),x,y)| (abs bordx > 213.5) && (abs bordy > 233.5) = (p1,(-dx,-dy), x-dx, y-dy)
                             | abs bordy > 233.5 = (p1,(dx,-dy), x+dx, y-dy)
                             | abs bordx > 213.5 = (p1,(-dx,dy), x-dx, y+dy)
                             | otherwise = (p1,(dx,dy), x+dx, y+dy)
                           where (bordx, bordy) = (x+dx,y+dy) -- Posição Seguinte

