#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Definição da estrutura para a lista ligada
typedef struct LInt {
    int valor;
    struct LInt *prox;
} NodoLInt, *LInt;

// Definição da estrutura para a árvore binária de procura
typedef struct palavras {
    char *palavra;
    int nOcorr;
    struct palavras *esq, *dir;
} Palavra, *Palavras;

// Funções
int paresImpares(int v[], int N) {
    int i,j, contaPares = 0;
    for (i = N; i > 0; i--) {
        for (j = 1; j < i; j++) {
            if (v[j-1] % 2 == 1){
                int temp = v[j];
                v[j] = v[j-1];
                v[j-1] = temp;
            }
        }
    }
    for (i = 0; v[i] % 2 == 0; i++) contaPares++;
    return contaPares;
}

void merge(LInt *r, LInt a, LInt b){
    if (a == NULL && b == NULL) return;
    if (b == NULL || a != NULL && a->valor < b->valor) {
        *r = a;
        merge(&(*r)->prox, a->prox, b);
    } else {
        *r = b;
        merge(&(*r)->prox, a, b->prox);
    }
}

void latino(int N, int m[N][N]) {
    for (int i = 0; i<N; i++) {
        for (int j = 0; j<N; j++){
            m[i][j] = 1 + (i + j) % N;
        }
    }
}

typedef struct nodo {
    int valor;
    struct nodo *pai, *esq, *dir;
} *ABin;

ABin next(ABin a) {
    if (a == NULL) return NULL;
    if (a->dir != NULL) {
        a = a->dir;
        while (a->esq != NULL) a = a->esq;
        return a;
    }
    else {
        ABin p = a->pai;
        while (p != NULL && a == p->dir) {
            a = p;
        }
        return p;
    }    
}

void rodaEsquerda(Palavras *a) {
    Palavras b = (*a)->dir;
    (*a)->dir = b->esq;
    b->esq = (*a);
    *a = b;
}

void rodaDireita(Palavras *a) {
    Palavras b = (*a)->esq;
    (*a)->esq = b->dir;
    b->dir = *a;
    *a = b;
}

int acrescentaPal(Palavras *p, char *pal) {
    if (*p == NULL) {
        *p = malloc (sizeof (struct palavras));
        (*p)->palavra = strdup (pal);
        (*p)->nOcorr = 1;
        (*p)->dir = (*p)->esq = NULL;
        return 1;
    }
    if (strcmp((*p)->palavra, pal) == 0){
        (*p)->nOcorr++;
        return (*p)->nOcorr;
    }
    else if (strcmp((*p)->palavra, pal) < 0) { // Se a palavra atual for lexicograficamente menor que a palavra a ser inserida, inserimos na subárvore direita
        int ocorr = acrescentaPal(&((*p)->dir), pal);
        if ((*p)->dir != NULL && (*p)->dir->nOcorr > (*p)->nOcorr) { // Se o filho direito tiver mais ocorrências que o pai, fazemos uma rotação à esquerda
            rodaEsquerda(p);
        }
        return ocorr;
    } else { // Se a palavra atual for lexicograficamente maior que a palavra a ser inserida, inserimos na subárvore esquerda
        int ocorr = acrescentaPal(&((*p)->esq), pal);
        if ((*p)->esq != NULL && (*p)->esq->nOcorr > (*p)->nOcorr) { // Se o filho esquerdo tiver mais ocorrências que o pai, fazemos uma rotação à direita
            rodaDireita(p);
        }
        return ocorr;
    }
}

// Função para imprimir a árvore de palavras (em ordem lexicográfica)
void printArvore(Palavras p) {
    if (p != NULL) {
        printArvore(p->esq);
        printf("%s - %d\n", p->palavra, p->nOcorr);
        printArvore(p->dir);
    }
}

int main() {
    printf("Exercício 1:\n");
    int v[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int tamanho = sizeof(v) / sizeof(v[0]);
    int pares = paresImpares(v, tamanho);
    printf("Número de pares: %d\n", pares);
    printf("Array organizado: ");
    for (int i = 0; i < tamanho; i++) {
        printf("%d ", v[i]);
    }
    printf("\n\n");

    printf("Exercício 2:\n");
    // Teste da função merge
    LInt a = NULL, b = NULL;
    a = malloc(sizeof(NodoLInt));
    a->valor = 1;
    a->prox = malloc(sizeof(NodoLInt));
    a->prox->valor = 3;
    a->prox->prox = NULL;

    b = malloc(sizeof(NodoLInt));
    b->valor = 2;
    b->prox = malloc(sizeof(NodoLInt));
    b->prox->valor = 4;
    b->prox->prox = NULL;
    
    LInt result = NULL;
    merge(&result, a, b);
    // Imprimindo a lista resultante
    printf("Lista resultante: ");
    while (result != NULL) {
        printf("%d ", result->valor);
        result = result->prox;
    }
    printf("\n\n");

    printf("Exercício 3:\n");
    // Teste da função latino
    int N = 3;
    int matriz[N][N];
    latino(N, matriz);
    printf("Matriz quadrada latina de dimensão %d:\n", N);
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            printf("%d ", matriz[i][j]);
        }
        printf("\n");
    }
    printf("\n");

    printf("Exercício 4:\n");
    // Defina a árvore binária de procura para testar a função next
    ABin raiz = malloc(sizeof(struct nodo));
    raiz->valor = 2;
    raiz->esq = malloc(sizeof(struct nodo));
    raiz->esq->valor = 1;
    raiz->esq->pai = raiz;
    raiz->esq->esq = raiz->esq->dir = NULL;
    raiz->dir = malloc(sizeof(struct nodo));
    raiz->dir->valor = 3;
    raiz->dir->pai = raiz;
    raiz->dir->esq = raiz->dir->dir = NULL;
    printf("Next of %d: %d\n\n", raiz->valor, next(raiz)->valor);

    printf("Exercício 5:\n");
    // Teste da função acrescentaPal
    Palavras arvore = NULL;

    // Teste da função acrescentaPal
    printf("Adicionando palavras:\n");
    printf("Ocorrências de 'hello': %d\n", acrescentaPal(&arvore, "hello"));
    printf("Ocorrências de 'world': %d\n", acrescentaPal(&arvore, "world"));
    printf("Ocorrências de 'hello': %d\n", acrescentaPal(&arvore, "hello"));
    printf("Ocorrências de 'hello': %d\n", acrescentaPal(&arvore, "hello"));
    printf("Ocorrências de 'world': %d\n", acrescentaPal(&arvore, "world"));
    printf("Ocorrências de 'programming': %d\n", acrescentaPal(&arvore, "programming"));
    printf("Ocorrências de 'programming': %d\n", acrescentaPal(&arvore, "programming"));
    printf("Ocorrências de 'programming': %d\n", acrescentaPal(&arvore, "programming"));

    // Imprimir a árvore de palavras
    printf("\nÁrvore de palavras (ordem lexicográfica):\n");
    printArvore(arvore);

    return 0;
}
