module Questao9e10 where
import Data.List
import Test.HUnit

type Coordenada = (Float,Float)
type Comprimento = Float
type Largura = Float
data Movimento = E | W | N | S deriving (Show,Eq)
type Mapa = (Comprimento, Largura)

distanciapontos :: Coordenada -> Coordenada -> Float
distanciapontos (a,b) (x,y) = sqrt ((a-x)^2+(b-y)^2)

fAux :: Mapa -> Coordenada -> Coordenada -- o resultado é a coordenada do canto mais próxima do jogador
fAux (c,l) (x,y) | distanciapontos (c,l) (x,y) > distanciapontos (0,0) (x,y) = (c,l)
                 | distanciapontos (0,l) (x,y) > distanciapontos (0,0) (x,y) = (0,l)
                 | distanciapontos (c,0) (x,y) > distanciapontos (0,0) (x,y) = (c,0)
                 | otherwise = (0,0)


f :: Mapa -> Coordenada -> [Movimento]
f (c,l) (x,y) | x < c' = E: f (c,l) (x+1,y)
              | x > c' = W: f (c,l) (x-1,y)
              | y < l' = N: f (c,l) (x,y+1)
              | y > l' = S: f (c,l) (x,y-1)
              | otherwise = []
      where (c',l') = fAux (c,l) (x,y)

testesf = test ["Sendo a janela c=10 e l=10. Se o jogador estiver em (1,1) o canto mais perto será a origem (0,0) então o jogador terá de andar uma vez para Sul e uma vez para Oeste"~: [S,W] ~=? f (10,10) (1,1),
                "Sendo a janela c=10 e l=10. Se o jogador estiver em (5,5), ou seja no centro do Mapa, o Joador deslocar-se-à para a Origem (0,0)." ~: [S,S,S,S,S,W,W,W,W,W] ~=? f (10,10) (5,5)
                   ]


{-| Esta função g recebe duas listas com posições de jogadores e retorna uma lista com as posições "juntas" das duas listas iniciais. As posições que são comuns aprecem um só personagem na lista final.

== Exemplo

>>> g [(0,0),(5,5)] [(0,0),(4,4)] = [(0,0),(4,4),(5,5)]

>>> g [(1,2)] [(1,1)] = [(1,2),(1,1)]

-}

gAux :: [Coordenada] -> [Coordenada]
gAux [] = []
gAux (h:t) | elem h t = gAux t
           | otherwise = h:gAux t

g :: [Coordenada] -> [Coordenada] -> [Coordenada]
g [] s = s
g l [] = l
g l s = gAux (l ++ s)