module EncontraPosicaoBlocos where

import LI12324
import Functions

tBls = (50,50) -- Tamanho dos Blocos do Mapa
colecionaveisteste = [(Martelo,(10.0,-10.0)),(Moeda,(50.0,-40.0))]

encontraPosicaoBlocoMatriz :: Bloco -> Posicao -> Mapa -> [Posicao]
encontraPosicaoBlocoMatriz _ _ (Mapa _ _ []) = []
encontraPosicaoBlocoMatriz b (x,y) (Mapa pd p ([]:t)) = encontraPosicaoBlocoMatriz b (x - fromIntegral (length (head t)) ,y-1) (Mapa pd p t)
encontraPosicaoBlocoMatriz b (x,y) (Mapa pd p ((bl:bls):t)) |b == bl = (x,y) : encontraPosicaoBlocoMatriz b (x+1,y) (Mapa pd p (bls:t))
                                                   |otherwise = encontraPosicaoBlocoMatriz b (x+1,y) (Mapa pd p (bls:t))


encontraPosicaoBlocoMapa :: Bloco -> Tamanho -> Mapa -> [Posicao]
encontraPosicaoBlocoMapa b (c,l) (Mapa pd p bls) = map (((+c/2) >< (+(-l)/2)) . ((*c) >< (*l))) (encontraPosicaoBlocoMatriz b (0,0) (Mapa pd p bls))

hitboxBlocos :: Bloco -> Mapa -> Tamanho -> [Hitbox]
hitboxBlocos b m@(Mapa _ _ bls) (c,l) = map (\(x,y) -> ((x-(c/2),y+(l/2)),(x+(c/2),y-(l/2)))) pos
  where
    pos = encontraPosicaoBlocoMapa b tBls m

hitboxcolecionaveis :: (Colecionavel,Posicao) -> Hitbox
hitboxcolecionaveis (c,(x,y)) = ((x - 25, y + 25), (x + 25, y - 25))

limitesBlocos :: Bloco -> Mapa -> [((Double,Double),(Double,Double))]
limitesBlocos b m@(Mapa _ _ bls) = map (split (+25*(-1)) (+25) >< split (+25*(-1)) (+25)) pbls
    where pbls = encontraPosicaoBlocoMapa b tBls m


encontraBlocoMatriz :: Bloco -> Posicao -> Mapa -> Bool
encontraBlocoMatriz b (x,y) m = (x,y) `elem` encontraPosicaoBlocoMatriz b (0,0) m

trocaNEsimoBloco :: Bloco -> Int -> Bloco -> Mapa -> Mapa
trocaNEsimoBloco b n bt (Mapa x y bls) = Mapa x y (separaLista separacao (trocaNEsimoBlocoAux b n bt (concat bls)))
  where separacao = length (head bls)

trocaNEsimoBlocoAux :: Bloco -> Int -> Bloco -> [Bloco] -> [Bloco]
trocaNEsimoBlocoAux b n bt (h:t) | b == h && n == 1 = bt:t
                                 | b == h && n /= 1 = h : trocaNEsimoBlocoAux b (n-1) bt t
                                 | otherwise = h : trocaNEsimoBlocoAux b n bt t

separaLista :: Int -> [a] -> [[a]]
separaLista _ [] = []
separaLista n l = take n l : separaLista n (drop n l)

mapa1 = Mapa ((10,10),Este) (425,-60)
       (stringParaMapa stringMapa1)

stringMapa1 = ["                         ",
               "                         ",
               "    PPPPPPPPPPPPPPPPPPPP ",
               "                     E   ",
               "                     E   ",
               "                     E   ",
               "  PPPPPPPPPP      PAPPP  ",
               "       E          E      ",
               "       E          E      ",
               "       E          E      ",
               " PPPPPPPPPPPAPPPPPPPPPP  ",
               "         e           E   ",
               "         e           E   ",
               "                     E   ",
               "                     E   ",
               "   PPPPPPPAPPPPPPPPAPPPP ",
               "      E         e        ",
               "      E                  ",
               "      E         e        ",
               "PPPPPPPPPPPPPPPPPPPPPPPPP"]


mapa2 = Mapa ((10,10),Este) (1400,-660)
        (stringParaMapa stringMapa2)

stringMapa2 = ["                              ",
               "                              ",
               "                              ",
               "    PPPPP    PPPPP            ",
               "        E      E              ",
               "        E      E              ",
               "       PPPPPPPPPP             ",
               "            E                 ",
               "            E       PPAPPPPPPP",
               "            E             E   ",
               "            E             E   ",
               "  PPPPPPPPAPPPP           E   ",
               "     E                    E   ",
               "     E                    E   ",
               "     E                PPPPPPP ",
               "     E                        ",
               "   PPPAPPPP                   ",
               "        E       PPPP          ",
               "        E         E      e    ",
               "        E         E      e    ",
               "PPPPPPPPPPPPPPPPPPPPPPPPPPPPPP"]

mapateste1 = Mapa ((10,10),Oeste) (10,10) blocos1 
blocos1 =  stringParaMapa ["PPPPPPPPPP"
            ,"          "
            ,"          "
            ,"PP    PPPP"
            ,"        E "
            ,"        E "
            ,"PPPPPPPPPP"
            ]
blocos2 =   ["PPPPPPPPPP"
            ,"          "
            ,"          "
            ,"PP    PPAP"
            ,"        E "
            ,"        E "
            ,"PAP PP PPP"
            ]

mapateste2 = Mapa ((10,10),Este) (10,10)
            (stringParaMapa blocos2)

mapateste = Mapa ((10,10),Este) (10,10)
            (stringParaMapa ["PPPPPPPPPP"
                            ,"          "
                            ,"          "
                            ,"PP    PPPP"
                            ,"        E "
                            ,"        E "
                            ,"PPPPPPPPPP"])

stringParaMapa :: [String] -> [[Bloco]]
stringParaMapa = map linha
    where
    linha :: String -> [Bloco]
    linha "" = []
    linha (h:t) |h == 'e' = EscadaIncompleta : linha t 
                |h == 'P' = Plataforma : linha t
                |h == 'E' = Escada : linha t
                |h == 'A' = Alcapao : linha t
                |h == ' ' = Vazio : linha t