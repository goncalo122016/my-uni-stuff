ghci Cp.hs

ghci> xor = uncurry (/=)
ghci> and = uncurry (&&)
ghci> f = xor . (and >< id)