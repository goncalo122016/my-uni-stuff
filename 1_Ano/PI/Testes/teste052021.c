#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

// Estrutura para a lista ligada de inteiros
typedef struct LInt {
    int valor;
    struct LInt *prox;
} NodoLInt, *LInt;

// Estrutura para a árvore binária
typedef struct nodo {
    int valor;
    struct nodo *pai, *esq, *dir;
} NodoABin, *ABin;

// Funções
int sumhtpo(int n) {
    if (n <= 0) return -1; // Verifica se n é válido

    int parcelas[100]; // Array para armazenar as parcelas
    int count = 0; // Contador de parcelas

    // Inicializa o primeiro elemento como n
    parcelas[count++] = n;

    // Calcula as parcelas até o 100º termo ou até n se tornar 1
    while (n != 1 && count < 100) {
        if (n % 2 == 0) {
            n = n / 2;
        } else {
            n = 1 + (3 * n);
        }
        parcelas[count++] = n; // Adiciona a próxima parcela à lista
    }

    // Verifica se foram calculadas menos de 100 parcelas
    if (count < 100) return -1;

    // Ordena as parcelas em ordem crescente
    for (int i = 0; i < count - 1; i++) {
        for (int j = i + 1; j < count; j++) {
            if (parcelas[i] > parcelas[j]) {
                int temp = parcelas[i];
                parcelas[i] = parcelas[j];
                parcelas[j] = temp;
            }
        }
    }

    // Retorna a 100ª menor parcela
    return parcelas[99];
}

int moda(int v[], int N, int *m);

int procura(LInt *l, int x){
    LInt atual, ant;
    for (atual = *l; atual != NULL && atual->valor != x; atual = atual->prox) ant = atual;
    if (atual == NULL) return 0;
    if (atual == *l) return 1;
    LInt temp = atual;
    ant->prox = atual->prox;
    temp->prox = *l;
    *l = temp;
    return 1;
}

int freeAB(ABin a) {
    if (a == NULL) return 0;
    int nodos = 1;
    nodos += freeAB(a->esq);
    nodos += freeAB(a->dir);
    free(a);
    return nodos;
}

void caminho(ABin a) {
    if (a == NULL) return;
    while (a->pai != NULL) {
        if (a->pai->esq == a) printf("esq\n");
        else printf("dir\n");
        a = a->pai;
    }
}

int main() {
    // Teste das funções
    printf("Exercício 1:\n");
    printf("Soma até a 100ª parcela para n = 100: %d\n", sumhtpo(100));

    //printf("\nExercício 2:\n");
    //int v1[] = {1, 2, 2, 3, 4, 4, 4, 5};
    //int moda_v1, freq_v1 = moda(v1, sizeof(v1) / sizeof(v1[0]), &moda_v1);
    //if (freq_v1 > 0) {
    //    printf("Moda: %d, Frequência: %d\n", moda_v1, freq_v1);
    //} else {
    //    printf("Conjunto vazio ou multimodal\n");
    //}
//
    printf("\nExercício 3:\n");

    // Criação da lista com três elementos
    LInt lista = malloc(sizeof(struct LInt));
    lista->valor = 1;
    lista->prox = malloc(sizeof(struct LInt));
    lista->prox->valor = 2;
    lista->prox->prox = malloc(sizeof(struct LInt));
    lista->prox->prox->valor = 3;
    lista->prox->prox->prox = NULL;

    // Teste da função procura
    procura(&lista, 1);

    // Impressão da lista após a busca e movimento do elemento 2 para o início
    printf("Lista após procurar e mover o 2 para o início: ");
    for (LInt atual = lista; atual != NULL; atual = atual->prox) {
        printf("%d ", atual->valor);
    }
    printf("\n");

    printf("\nExercício 4:\n");
    ABin arvore = malloc(sizeof(NodoABin));
    arvore->valor = 1;
    arvore->esq = malloc(sizeof(NodoABin));
    arvore->esq->valor = 2;
    arvore->esq->pai = arvore;
    arvore->esq->esq = arvore->esq->dir = NULL;
    arvore->dir = malloc(sizeof(NodoABin));
    arvore->dir->valor = 3;
    arvore->dir->pai = arvore;
    arvore->dir->esq = arvore->dir->dir = NULL;
    int nodos_liberados = freeAB(arvore);
    printf("Número de nodos liberados: %d\n", nodos_liberados);

    printf("\nExercício 5:\n");
    // Vamos criar uma árvore binária simples para testar a função caminho
    ABin raiz = malloc(sizeof(NodoABin));
    raiz->valor = 1;
    raiz->pai = NULL;
    raiz->esq = malloc(sizeof(NodoABin));
    raiz->esq->valor = 2;
    raiz->esq->pai = raiz;
    raiz->esq->esq = raiz->esq->dir = NULL;
    raiz->dir = malloc(sizeof(NodoABin));
    raiz->dir->valor = 3;
    raiz->dir->pai = raiz;
    raiz->dir->esq = raiz->dir->dir = NULL;
    printf("Caminho para o nodo 3:\n");
    caminho(raiz->dir);

    return 0;
}