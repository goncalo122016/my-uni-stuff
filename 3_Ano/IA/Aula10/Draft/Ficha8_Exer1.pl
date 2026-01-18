%LICENCIATURA EM ENGENHARIA INFORMATICA
%MESTRADO integrado EM ENGENHARIA INFORMATICA

%Inteligencia Artificial
%2024/25

%Draft Ficha 8 Exercicio 1


aluno(1,joao,m).
aluno(2,antonio,m).
aluno(3,carlos,m).
aluno(4,luisa,f).
aluno(5,maria,f).
aluno(6,isabel,f).

curso(1,lei).
curso(2,miei).
curso(3,lcc).

%disciplina(cod,sigla,ano,curso)
disciplina(1,ed,2,1).
disciplina(2,ia,3,1).
disciplina(3,fp,1,2).

%inscrito(aluno,disciplina)
inscrito(1,1).
inscrito(1,2).
inscrito(5,3).
inscrito(5,5).
inscrito(2,5).

%nota(aluno,disciplina,nota)
nota(1,1,15).
nota(1,2,16).
nota(1,5,20).
nota(2,5,10).
nota(3,5,8).

%copia
copia(1,2).
copia(2,3).
copia(3,4).

%

alunos_sem_disciplina(R) :- findall(Aluno, (aluno(Id,Aluno,_), not(inscrito(Id,_))) , R). 

%

alunos_sem_disciplina2(R) :- findall(Aluno, (aluno(Id,Aluno,_), \+ (inscrito(Id,D), disciplina(D,_,_,_))), R).

%

somaL([],0). 
somaL([H | T],Soma) :- somaL(T, G), Soma is G+H. 

len([], 0). 
len([_|T], L) :- len(T, L1), L is L1 + 1. 

media(Aluno, R) :- findall(Nota, (aluno(Id,Aluno,_), nota(Id,_, Nota)) , L), somaL(L, S), len(L, Len), Len > 0, R is S / Len.

%

mediaGlobal(R) :- findall(Nota, (nota(_,_,Nota)), Notas), somaL(Notas, S), len(Notas, L), R is S/L.

aluno_acima(R) :- findall(Aluno, (aluno(Id,Aluno,_), media(Aluno,Media), mediaGlobal(MG), Media > MG), R).

%

alunos_copiaram(R) :- findall(Aluno, (copia(Id,_), aluno(Id,Aluno,_)), R).

%

copiou_de(X,Y) :- copia(X,Y).
copiou_de(X,Y) :- copia(X,Z), copiou_de(Z,Y).

alunos_que_copiaram_de(Aluno, R) :- aluno(Id,Aluno,_), findall(A, (aluno(IdA, A, _), copiou_de(IdA, Id)), R).

%

mapToNome([], []).
mapToNome([Id|T], [Nome|R]) :- aluno(Id,Nome,_) , mapToNome(T, R).
mapToNome([Id|T], R) :- \+ aluno(Id,_,_) , mapToNome(T, R).