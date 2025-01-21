module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play (InWindow "MAIN2" (500,500) (0,0))
        white
        50
        estadoinicial
        draw 
        reageEvento 
        reageTempo 

data Estado = Estado {posicao :: (Float, Float)
                     ,velocidade :: (Float, Float)}

estadoinicial :: Estado
estadoinicial = Estado (0,0) (30,-50)

colideTopo :: Estado -> Bool
colideTopo e = snd (posicao e) >= 225

colideBaixo :: Estado -> Bool
colideBaixo e = snd (posicao e) <= (-225)

colideDireita :: Estado -> Bool
colideDireita e = fst (posicao e) >= 225

colideEsquerda :: Estado -> Bool
colideEsquerda e = fst (posicao e) <= (-225)

draw :: Estado -> Picture
draw e = Translate x y $ rectangleSolid 50 50 
        where (x,y) = posicao e

atualizaEstado :: Float -> Estado -> Estado
atualizaEstado t e = Estado (x+dx,y+dy) (vx,vy)
                    where (x,y) = posicao e
                          (vx, vy) = velocidade e
                          dx = t * vx
                          dy = t * vy

reageEvento :: Event -> Estado -> Estado
reageEvento _ e = e 

reageTempo :: Float -> Estado -> Estado
reageTempo t e | colideTopo e' || colideBaixo e' = Estado (x,y) (vx,-vy)
               | colideDireita e' || colideEsquerda e' = Estado (x,y) (-vx,vy)
               | otherwise = atualizaEstado t e
                    where e' = atualizaEstado t e
                          (x,y) = posicao e'
                          (vx, vy) = velocidade e'