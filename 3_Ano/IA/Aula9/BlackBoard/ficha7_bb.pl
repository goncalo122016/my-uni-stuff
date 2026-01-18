%LICENCIATURA EM ENGENHARIA INFORMATICA
%MESTRADO integrado EM ENGENHARIA INFORMATICA

%Inteligencia Artificial
%2025/26

%Draft Ficha 7


% Parte I
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Operacoes aritmeticas

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado soma: X,Y,Z,Soma -> {V,F}

soma( X,Y,Z,Soma ) :-
    Soma is X+Y+Z.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado somaL: L ,Soma -> {V,F}
somaL([],0).
somaL([H | T],Soma) :-  somaL(T, G), Soma is G+H.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maior: X,Y,R -> {V,F}
maior(X,Y,X) :- X>=Y.
maior(X,Y,Y).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado maior: Lista, M, Resultado -> {V,}
auxMax([], A, A).
auxMax([H|T], A, Max) :- H > A, auxMax(T, H, Max).
auxMax([H|T], A, Max) :- H =< A, auxMax(T, A, Max).

maiorLista([], 0).
maiorLista([H|T], Max) :- auxMax(T, H, Max).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Quantidade de elementos de uma lista.
len([_], 1) :- !.
len([_|T], L) :- len(T, L1), L is L1 + 1.

len2([], 0).
len2([_|T], L) :- len(T, L1), L is L1 + 1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Tamanho de uma Lista


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Média aritmética de uma lista

media([], 0).
media(L, M) :- somaL(L, S), len(L, Len), M is S / Len.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% verificar se um numero é par

par(0).
par(X) :-
    X > 0,
    X2 is X - 2,
    par(X2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado impar: N -> {V,F}

impar(1).
impar(X) :-
    X > 1,
    X2 is X - 2,
    impar(X2).

% Parte II--------------------------------------------------------- - - - - -

ordena([], []).
ordena([H|T], ListaOrd) :- ordena(T, TOrdenada), 
    					   insere_ordenado(H, TOrdenada, ListaOrd).

% Caso base: inserir num lista vazia
insere_ordenado(X, [], [X]).

% Se X é menor ou igual ao primeiro elemento, coloca à frente
insere_ordenado(X, [H|T], [X,H|T]) :-
    X =< H.

% Caso recursivo: procurar posição onde X deve ficar
insere_ordenado(X, [H|T], [H|R]) :-
    X > H,
    insere_ordenado(X, T, R).
    
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento,Lista -> {V,F}

pertence( X,[X|L] ).
pertence( X,[Y|L] ) :- pertence( X,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista,Comprimento -> {V,F}



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado quantos: Lista,Comprimento -> {V,F}

quantos(_, [], 0).
quantos(X, [X|T], R) :- quantos(X, T, R1), R is R1 + 1.

quantos(X, [H|T], R) :- quantos(X, T, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado apagar: Elemento,Lista,Resultado -> {V,F}

apagar(_, [], []).
apagar(X, [X|T], T).
apagar(X, [H|T], [H|R]) :- X \= H, apagar(X, T, R).
          
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado apagatudo: Elemento,Lista,Resultado -> {V,F}

apagaTudo(_, [], []).
apagaTudo(X, [X|T], R) :- apagaTudo(X, T, R).
apagaTudo(X, [H|T], [H|R]) :- apagaTudo(X, T, R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado adicionar: Elemento,Lista,Resultado -> {V,F}

adicionar(X, L, L) :- pertence(X, L), !.
adicionar(X, L, [X|L]).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado concatenar: Lista1,Lista2,Resultado -> {V,F}

concatenar([], L, L).
concatenar([H|T], L2, [H|R]) :- concatenar(T, L2, R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado inverter: Lista,Resultado -> {V,F}

inverter([], []).
inverter([H|T], R) :- inverter(T, RT), concatenar(RT, [H], R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado sublista: SubLista,Lista -> {V,F}

sub(S,L) :- concatenar(L1,L3, L), concatenar(S, L2, L3).


subLista([], _).
subLista([H|T], [H|TL]) :- prefixo(T, TL).
subLista(S, [_|TL]) :- subLista(S, TL).

% prefixo(P, L): P é prefixo de L
prefixo([], _).
prefixo([H|T], [H|TL]) :- prefixo(T, TL).