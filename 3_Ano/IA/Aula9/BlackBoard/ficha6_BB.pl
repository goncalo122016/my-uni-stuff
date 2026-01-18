
%LICENCIATURA EM ENGENHARIA INFORMÁTICA
%MESTRADO integrado EM ENGENHARIA INFORMÁTICA

%Inteligência Artificial
%2025/26

%Draft Ficha 6


% Extensao do predicado filho: Filho,Pai -> {V,F}

filho( joao,jose ).
filho( jose,manuel ).
filho( carlos,jose ).
filho( manuel, antonio ).

masculino(joao).
masculino(jose).

feminino(maria).
feminino(joana).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pai: Pai,Filho -> {V,F}

pai( P,F ) :- filho( F,P ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado avo: Avo,Neto -> {V,F}

avo(A,N) :- pai(A,X) , pai(X,N).
neto(N,A) :- avo(A,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado bisavo: Bisavo,Bisneto -> {V,F}

bisavo( B,Bn ) :- pai(B, X) , avo(X, Bn).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: Descendente,Ascendente -> {V,F}

descendente(D,A) :- filho(D,A).
descendente(D,A) :- filho(D,P) , descendente(P,A). 


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado descendente: Descendente,Ascendente,Grau -> {V,F}

descendente(D,A,1) :- filho(D,A).
descendente(D,A,N) :- filho(D,X), descendente(X,A,G), N is G + 1.
