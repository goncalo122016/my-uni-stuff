module Questao8 where

type Livro = (Titulo, Autores, Ano)
type Titulo = String
type Autores = [Nome]
type Nome = String
type Ano = Int

{-| Esta função recebe uma lista do tipo Livro e retorna a lista dos nomes de todos os Autores de todos os Livros da lista.

== Exemplo

>>> f [] = []

>>> f [("O Senhor Dos Anéis",["Joao","Joana"],2002),("A praia",["Antonio"],2007)] = ["Joao","Joana","Antonio"]

-}

f :: [Livro] -> Autores
f l = concatMap (\(x,y,z)-> y) l
