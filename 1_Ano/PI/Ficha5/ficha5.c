#include <stdio.h>
#include <math.h>
#include <string.h>

typedef struct aluno {
    int numero;
    char nome[100];
    int miniT [6];
    float teste;
} Aluno;



int nota (Aluno a) {
    int i;
    int miniNota = 0, notaFinal;
    for (i=0; i<6; i++) {
        miniNota += a.miniT[i];
    }
    if (miniNota < 3) return 0;
    notaFinal = a.teste * 0.8 + (((double)miniNota / 12) * 20) * 0.2;
    if (notaFinal < 9.5) return 0;
    return round(notaFinal);
}

void imprimeAluno (Aluno *a){
    int i;
    printf ("%-5d %s (%d", a->numero, a->nome, a->miniT[0]);
    for(i=1; i<6; i++) printf (", %d", a->miniT[i]);
    printf (") %5.2f %d\n", a->teste, nota(*a));
}

int procuraNumLinear (int num, Aluno t[], int N) {
    for (int i = 0; i < N; i++) {
        if (num == t[i].numero) return i;
    }
    return -1;
}

void insereNum(Aluno x, Aluno n[], int i) {
    while (i > 0 && n[i-1].numero > x.numero) {
        n[i] = n[i-1];
        i--;
    }
    n[i] = x;
}

void ordenaPorNum (Aluno t [], int N) {
    for (int i = 0; i<N; i++) {
        insereNum(t[i], t, N);
    }
}

void ordenaPorNum1 (Aluno t [], int N) {
    int i, j;
    Aluno aux;
    for (i = 1; i<N; i++) {
        aux = t[i];
        for (j = i; j>0 && t[j-1].numero > aux.numero; j--) t[j] = t[j-1];
        t[j] = aux;
    }
}

int procuraNumBinaria (int x, Aluno a[], int N) {	
    int l = 0, u = N-1, m;	
    while (l <= u) {	
        m = (l+u) / 2;	
        if (a[m].numero == x) return m;	
        if (a[m].numero < x) l = m+1;	
        else u = m-1;	
    }	
    return -1;	
}

void imprimeTurmaInd (int ind[], Aluno t[], int N) {
    int i;
    for (i = 0; i<N; i++) imprimeAluno (&(t[ind[i]]));
}

int procuraNumInd (int num, int ind[], Aluno t[], int N){
    return -1;
}

void criaIndPorNum (Aluno t [], int N, int ind[]){
    int i;
    for (i = 0; i<N; i++) ind[i] = i;
    int j, aux;
    for (i = 1; i<N; i++) {
        aux = ind[i];
        for (j = i; j>0 && t[ind[j-1]].numero > t[aux].numero; j--) ind[j] = ind[j-1];
        ind[j] = aux;
    }
}

void criaIndPorNome (Aluno t [], int N, int ind[]){
    int i;
    for (i = 0; i<N; i++) ind[i] = i;
    int j, aux;
    for (i = 1; i<N; i++) {
        aux = ind[i];
        for (j = i; j>0 && strcmp(t[ind[j-1]].nome, t[aux].nome) > 0; j--) ind[j] = ind[j-1];
        ind[j] = aux;
    }
}


int main() {
    Aluno Turma1 [7] = {{4444, "André", {2,1,0,2,2,2}, 10.5}
                       ,{3333, "Paulo", {0,0,2,2,2,1},  8.7}
                       ,{8888, "Carla", {2,1,2,1,0,1}, 14.5}
                       ,{2222, "Joana", {2,0,2,1,0,2},  3.5}
                       ,{7777, "Maria", {2,2,2,2,2,1},  5.5}
                       ,{6666, "Bruna", {2,2,2,1,0,0}, 12.5}
                       ,{5555, "Diogo", {2,2,1,1,1,0},  8.5}
                       } ;
    int indNome [7], indNum [7];
    int i;

    printf ("\n-------------- Testes --------------\n");
    
    ordenaPorNum (Turma1, 7);

    ordenaPorNum1 (Turma1, 7);
    printf ("procura 5555: %d \n", procuraNumBinaria (5555, Turma1, 7));
    printf ("procura 9999: %d \n", procuraNumBinaria (9999, Turma1, 7));

    for (i=0; i<7; imprimeAluno (Turma1 + i++));
    
    //criaIndPorNum (Turma1, 7, indNum);
    
    // criaIndPorNome (Turma1, 7, indNome);

    // imprimeTurmaInd (indNum, Turma1, 7);
    // imprimeTurmaInd (indNome, Turma1, 7);

    // printf ("procura 5555:%d \n",  procuraNumInd (5555, indNum, Turma1, 7));
    // printf ("procura 9999:%d \n",  procuraNumInd (9999, indNum, Turma1, 7));

    //printf ("\n---------- Fim dos Testes ----------\n");
    
    return 0;
}
