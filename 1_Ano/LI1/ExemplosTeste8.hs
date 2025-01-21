module ExemplosTeste8 where

type Livro = (Titulo, Autores, Ano)
type Titulo = String
type Autores = [Nome]
type Nome = String
type Ano = Int

fAux :: Livro -> Bool
fAux (t,a,an) | length a == 1 = True
              | otherwise = False

f :: [Livro] -> [(Titulo, Nome)]
f l = map (\(x,y,z) -> (x,unwords y)) (filter fAux l)

gAux :: String -> String -> Bool
gAux p l = p `elem` words l

g :: [String] -> String -> [String]
g l p = filter (gAux p) l

hAux :: Int -> String -> Bool
hAux n f = n < length (words f)

h :: [String] -> Int -> [String]
h l n = filter (hAux n) l

iAux :: Int -> String -> Bool
iAux n f = n < length (words f)

i :: [String] -> Int -> Bool
i l n = all (iAux n) l

type Equipa = String
type Golos = Int
type Jogo = ((Equipa,Golos), (Equipa, Golos))
type Jornada = [Jogo]

perdedoras :: Jornada -> [Equipa]
perdedoras l = filter (/="EMPATE") (map selperdedora l)

selperdedora :: Jogo -> Equipa
selperdedora ((e1,g1),(e2,g2)) | g1==g2 = "EMPATE"
                               | g1>g2 = e2
                               | otherwise = e1

type Rectangulo = (Double, Double)
type Figuras = [Rectangulo]

escala :: Figuras -> Double -> Figuras
escala l x = map (aplicaEscala x) l

aplicaEscala :: Double -> Rectangulo -> Rectangulo
aplicaEscala e (c,l) = (c*e,l*e)
