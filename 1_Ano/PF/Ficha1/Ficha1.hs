module Ficha1 where
import Data.Char

perimetro :: Double -> Double
perimetro r = 2*pi*r

primUlt :: [a]->(a,a)
primUlt l= (head l,last l)

multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n ==0 then True else False

max2 :: Int -> Int -> Int
max2 x y = if x>y then x else y

max3 :: Int -> Int -> Int -> Int
max3 x y z = if max2 x y >= max2 y z then max2 x y else max2 y z

raizes :: Double -> Double -> Double -> [Double]
raizes a b c = let d = b^2-4*a*c
               in if d<0 then []
                  else if d==0 then [(-b)/(2*a)]
                  else [((-b)+sqrt d)/(2*a),((-b)-sqrt d)/(2*a)]

data Hora = H Int Int deriving (Show,Eq)

horateste :: Hora -> Bool
horateste (H x y) = (x >= 0 && x <= 23) && (y >= 0 && y <= 59)

depois :: Hora -> Hora -> Bool
depois (H x1 y1) (H x2 y2) = x1>x2 || (x1==x2 && y1>y2)

hormin :: Hora -> Int
hormin (H x y) = x*60+y

minhor :: Int -> Hora
minhor m = (H (div m 60) (mod m 60))

dif :: Hora -> Hora -> Int
dif (H x1 y1) (H x2 y2) = if depois (H x1 y1) (H x2 y2) == False then (x2*60-x1*60)+(y2-y1) else (x1*60-x2*60)+(y1-y2)

add :: Int -> Hora -> Hora
add a (H x y) = minhor (hormin (H x y) + a)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop Verde = False
stop Amarelo = True

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = if s1 == Vermelho && s2 == Vermelho then True else False

data Ponto = Cartesiano Double Double 
           | Polar Double Double deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar d a) = d*cos(a)

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar d a) = d*sin(a)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2+y^2)
raio (Polar d a) = d

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (y/x)
angulo (Polar d a) = a

dist ::  Ponto -> Ponto -> Double
dist p q = sqrt ((posx p - posx q)^2 + (posy p - posy q)^2)

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo (Cartesiano x y) r) = False
poligono (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = True
poligono (Triangulo (Cartesiano x1 y1) (Cartesiano x2 y2) (Cartesiano x3 y3)) = True

vertices :: Figura -> [Ponto]
vertices (Circulo (Cartesiano x y) r) = []
vertices (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [(Cartesiano x1 y1), (Cartesiano x2 y2)]

vertices1 :: Figura -> [(Double,Double)]
vertices1 (Circulo (Cartesiano x y) r) = []
vertices1 (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [(x1,y1), (x2,y2)]

area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist p1 p2
                                b = dist p2 p3
                                c = dist p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
       in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo p1 r) = r^2*pi
area (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = if x1==x2 || y1==y2 then 0
                                                         else c*l
                                                         where c= (dist (Cartesiano x1 y1) (Cartesiano x1 y2))
                                                               l= (dist (Cartesiano x1 y1) (Cartesiano x2 y1))
perimetrof :: Figura -> Double
perimetrof (Triangulo p1 p2 p3) = let a = dist p1 p2
                                      b = dist p2 p3
                                      c = dist p3 p1
                                 in a+b+c
perimetrof (Circulo p1 r) = r*2*pi
perimetrof (Retangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = if x1==x2 || y1==y2 then 0
                                                         else 2*c+2*l
                                                         where c= (dist (Cartesiano x1 y1) (Cartesiano x1 y2))
                                                               l= (dist (Cartesiano x1 y1) (Cartesiano x2 y1))




letra :: Int -> Char
letra n = if (n>=65 && n<=90) || (n>=97 && n<=122)
          then chr n
          else '-' 