module Aula2 where

data Movimento = Norte | Sul | Este | Oeste deriving Show
type Ponto = (Double, Double)

move :: Ponto -> Movimento -> Ponto
move (x,y) Norte = (x,y+1)
move (x,y) Sul = (x,y-1)
move (x,y) Oeste = (x-1,y)
move (x,y) Este = (x+1,y)

dist :: Ponto -> Ponto -> Double
dist (x1,y1) (x2,y2) = sqrt (x1-x2)^2+(y1-y2)^2

janela :: Ponto -> Double -> Ponto
janela (x,y) j = (x,y-j)

janela' :: Ponto -> Double -> Ponto
janela' (x,y) j = (x-(j/2),y-(j/2))


type Velocidade = Double
type Tempo = Double

movimx :: Ponto -> Velocidade -> Tempo -> Ponto
movimx (x,y) v t = (x+d,y)
                  where d = if v==0 then 0 else v*t

movimy :: Ponto -> Velocidade -> Tempo -> Ponto
movimy (x,y) v t = (x,y+d)
                  where d = if v==0 then 0 else v*t

type Velocidades = (Double, Double)

movimento :: Ponto -> Velocidades -> Tempo -> Ponto
movimento (x,y) (vx,vy) t = (fst(movimx (x,y) vx t), fst (movimy (x,y) vy t))

data Figura = Circulo Ponto Double 
            | Retangulo Ponto Ponto deriving (Show,Eq)

dentro :: Ponto -> Figura -> Bool
dentro (a,b) (Circulo (x,y) r) = dist (a,b) (x,y) <= r
dentro (a,b) (Retangulo (x1,y1) (x2,y2)) = (x1<=a && x2>=a) && (y1<=b && y2>=b)

--2
type Nome = String
type Coordenada = (Double, Double)
data Moviment= N | S | E | W deriving (Show,Eq)
type Movimentos = [Moviment]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao p [] = p
posicao (Pos n (x,y)) m | head m == N = posicao (Pos n (x,y+1)) (tail m)
                        | head m == S = posicao (Pos n (x,y-1)) (tail m)
                        | head m == E = posicao (Pos n (x+1,y)) (tail m)
                        | head m == W = posicao (Pos n (x-1,y)) (tail m)

pessoasNorte :: [PosicaoPessoa] -> Maybe PosicaoPessoa
pessoasNorte [] = Nothing
pessoasNorte (h@(Pos _ (x,y)):s@(Pos _ (x1,y1)):t) | y<=y1 = Just (pessoasNorte (h:t))
                                                   | otherwise = Just (pessoasNorte (s:t))
