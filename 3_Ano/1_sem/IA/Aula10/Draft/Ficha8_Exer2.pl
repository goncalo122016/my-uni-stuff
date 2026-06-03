%LICENCIATURA EM ENGENHARIA INFORMATICA
%MESTRADO integrado EM ENGENHARIA INFORMATICA

%Inteligencia Artificial
%2024/25

%Draft Ficha 8 Exercicio 2


%biblioteca(id, nome, localidade)

biblioteca(1, uminhogeral, braga).
biblioteca(2, luciocracveiro, braga).
biblioteca(3, municipal, porto).
biblioteca(4, publica, viana).
biblioteca(5, ajuda, lisboa).
biblioteca(6, cidade, coimbra).


%livros( id, nome, biblioteca)

livros(1, gameofthrones, 1). 
livros(2, codigodavinci, 2).
livros(3, setimoselo, 1).
livros(4, fireblood, 4).
livros(5, harrypotter, 6).
livros(6, senhoradosneis, 7).
livros(7, oalgoritmomestre, 9).

%leitores(id, nome, genero)

leitores(1, pedro, m).
leitores(2, joao, m).
leitores(3, lucia, f).
leitores(4, sofia, f).
leitores(5, patricia, f).
leitores(6, diana, f).

%requisicoes(id_requisicao,id_leitor, id_livro, data(A,M,D)

requisicoes(1,2,3,data(2022,5,17)).
requisicoes(2,1,2,data(2022,7,10)).
requisicoes(3,1,3,data(2021,11,2)).
requisicoes(4,1,4,data(2022,2,1)).
requisicoes(5,5,3,data(2022,4,23)).
requisicoes(6,4,2,data(2021,3,9)).
requisicoes(7,4,1,data(2022,5,5)).
requisicoes(8,2,6,data(2021,7,18)).
requisicoes(9,5,7,data(2022,4,12)).


%devolucoes(id_requisicao, data(A,M, D))


devolucoes(2, data(2022, 7,26)).
devolucoes(4, data(2022,2,4)).
devolucoes(5, data(2022, 6, 13)).
devolucoes(1, data(2022, 5, 23)).
devolucoes(6, data(2022, 4, 9)).

% Quantos leitores do sexo feminino existem representados na base de conhecimentos;
feminino(R) :- findall(Genero, (leitores(_,_,Genero), Genero == f), L), length(L, R).

femininoNomes(R) :- findall(Nome, (leitores(_,Nome,Genero), Genero == f), R).

% Quais os livros que foram requisitados por leitores, mas que não se encontram associados a nenhuma 
% biblioteca da base de conhecimento;
livros_sem_biblio(R) :- findall(Nome, (requisicoes(_,_,Livro,_), livros(Livro, Nome, Biblio), not(biblioteca(Biblio,_,_))), R).

% Indique quais os livros e os respetivos leitores que efetuaram requisições em bibliotecas localizadas em Braga;
procura_braga(R) :- findall((Nome,Livro), (requisoes(_,ID_Leitor,ID_Livro, _), leitores(ID_Leitor, Nome,_), livro(ID_Livro,Livro,ID_Biblio), biblioteca(ID_Biblio,_,braga)), R).

% 4) Quais os livros que não tiveram nenhuma requisição. Para esta questão, assuma requisição de livros que 
% se encontram ou não em alguma biblioteca;
livrosSemRequisicao(R) :- findall(Livro, requisicoes(_,_,Livro,_), L), findall(Livros, (livros(Id,Livros,_), not(member(Id,L))), R).

% 5) Apresente a lista de livros, e a respetiva data de requisição, que tenham sido pedidos em 2022;
livrosPedidos2022(R) :- findall((Livro, Data), (requisicoes(_,_,Id,Data), Data = data(2022,_,_), livros(Id,Livro,_)), R).

% 6) Que leitores requisitaram li vros no Verão. Assuma que o Verão se encontra compreendido entre 
% Julho(7) e Setembro(9);
leitoresVerao(R) :- findall(Leitor, (requisicoes(_,Id,_,data(_,M,_)), M >= 7, M =< 9, leitores(Id,Leitor,_)), R).

% Indique quais os leitores, que entregaram um livro depois da data limite.
antes(DataR, DataD) :- DataR = data(Ano, Mes, DiaR), DataD = data(Ano, Mes, DiaD), DiaD > DiaR + 15.

leitores_em_falta(R) :- findall(Leitor, (devolucoes(Id,DataD), requisicoes(Id,Id_Leitor,_,DataR), antes(DataR, DataD), leitores(Id_Leitor, Leitor, _)), R).