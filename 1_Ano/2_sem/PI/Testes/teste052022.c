#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Definições dos tipos de dados
typedef struct LInt_nodo {
    int valor;
    struct LInt_nodo *prox;
} *LInt;

typedef struct ABin_nodo {
    int valor;
    struct ABin_nodo *esq, *dir;
} *ABin;

// Exercício 1

void swap(int v[], int i, int j) {
    int aux = v[i];
    v[i] = v[j];
    v[j] = aux;
}

void bsort(int v[], int n) {
    int i, j;
    for (i = n; i>0; i--) {
        for (j = 1; j<i; j++)
            if (v[j-1] > v[j]) swap(v, j-1, j);
    }
}

int nesimo(int a[], int N, int i) {
    bsort (a,N);
    return a[i-1];
}

// Exercício 2
LInt removeMaiores(LInt l, int x) {
    if (l == NULL) return NULL;
    LInt atual = l;
    for (atual; atual->prox->valor <= x && atual->prox != NULL; atual = atual->prox);
    atual->prox = NULL;
    return l;
}

// Exercício 3
LInt caminho(ABin a, int x) {
    if (a == NULL) return NULL;
    LInt nova = malloc(sizeof(struct LInt_nodo));
    nova->valor = a->valor;
    if (a->valor == x) {
        nova->prox = NULL;
        return nova;
    } else if (a->valor < x) {
        nova->prox = caminho(a->dir, x);
        return nova;
    } else {
        nova->prox = caminho(a->esq, x);
        return nova;
    }
}

// Exercício 4
void inc(char s[]) {
    int len = strlen(s), i;
    for (i = len-1; i >= 0 && s[i] == '9'; i--) s[i] = '0';
    if (i >= 0) 
        s[i]++;
    else {
        memmove(s + 1, s, len);
        s[0] = '1';
    }
}

// Exercício 5
int sacos(int p[], int N, int C) {
    int num_sacos = 0, capacidade_atual = 0;

    for (int i = 0; i < N; i++) {
        if (capacidade_atual + p[i] <= C) {
            capacidade_atual += p[i];
        } else {
            num_sacos++;
            capacidade_atual = p[i];
        }
    }

    if (capacidade_atual > 0) {
        num_sacos++;
    }
    return num_sacos;
}


// Função para criar um novo nó
LInt novoNo(int valor) {
    LInt novo = (LInt)malloc(sizeof(struct LInt_nodo));
    if (novo != NULL) {
        novo->valor = valor;
        novo->prox = NULL;
    }
    return novo;
}

// Função para inserir um novo nó no final da lista
LInt inserirNoFinal(LInt lista, int valor) {
    LInt novo = novoNo(valor);
    if (novo == NULL) {
        return lista;
    }
    if (lista == NULL) {
        return novo;
    }
    LInt atual = lista;
    while (atual->prox != NULL) {
        atual = atual->prox;
    }
    atual->prox = novo;
    return lista;
}

// Função para criar uma nova árvore binária de busca
ABin novoNoArvore(int valor) {
    ABin novo = (ABin)malloc(sizeof(struct ABin_nodo));
    if (novo != NULL) {
        novo->valor = valor;
        novo->esq = NULL;
        novo->dir = NULL;
    }
    return novo;
}

// Função para inserir um novo nó na árvore binária de busca
ABin inserir(ABin raiz, int valor) {
    if (raiz == NULL) {
        return novoNoArvore(valor);
    }
    if (valor < raiz->valor) {
        raiz->esq = inserir(raiz->esq, valor);
    } else if (valor > raiz->valor) {
        raiz->dir = inserir(raiz->dir, valor);
    }
    return raiz;
}

// Função para liberar a memória alocada para a lista encadeada
void liberarLista(LInt lista) {
    LInt atual = lista;
    while (atual != NULL) {
        LInt temp = atual;
        atual = atual->prox;
        free(temp);
    }
}

// Função para liberar a memória alocada para a árvore binária de busca
void liberarArvore(ABin raiz) {
    if (raiz != NULL) {
        liberarArvore(raiz->esq);
        liberarArvore(raiz->dir);
        free(raiz);
    }
}

int main() {
    // Teste do exercício 1
    int arr[] = {3, 1, 4, 1, 5, 9, 2, 6, 5};
    int n = sizeof(arr) / sizeof(arr[0]);
    int i = 3;
    printf("O %d-ésimo menor elemento é: %d\n", i, nesimo(arr, n, i));

    // Teste do exercício 2
    LInt lista = NULL;
    lista = inserirNoFinal(lista, 1);
    lista = inserirNoFinal(lista, 2);
    lista = inserirNoFinal(lista, 3);
    lista = inserirNoFinal(lista, 5);
    lista = inserirNoFinal(lista, 7);

    int x = 4;
    printf("Lista original: ");
    LInt atual = lista;
    while (atual != NULL) {
        printf("%d ", atual->valor);
        atual = atual->prox;
    }
    printf("\n");

    printf("Lista após remover elementos maiores que %d: ", x);
    LInt novaLista = removeMaiores(lista, x);
    atual = novaLista;
    while (atual != NULL) {
        printf("%d ", atual->valor);
        atual = atual->prox;
    }
    printf("\n");

    // Teste do exercício 3
    ABin arvore = NULL;
    arvore = inserir(arvore, 5);
    arvore = inserir(arvore, 3);
    arvore = inserir(arvore, 8);
    arvore = inserir(arvore, 2);
    arvore = inserir(arvore, 4);
    arvore = inserir(arvore, 7);
    arvore = inserir(arvore, 9);

    int valor = 7;

    LInt cam = caminho(arvore, valor);

    printf("Caminho até %d: ", valor);
    while (cam != NULL) {
        printf("%d ", cam->valor);
        cam = cam->prox;
    }
    printf("\n");

    liberarArvore(arvore);

    // Teste do exercício 4
    char str1[] = "123";
    inc(str1);
    printf("Incremento de '123': %s\n", str1);

    char str2[] = "199";
    inc(str2);
    printf("Incremento de '199': %s\n", str2);

    // Teste do exercício 5
    int pesos1[] = {3, 6, 2, 1, 5, 7, 2, 4, 1};
    int capacidade1 = 10;
    printf("Número mínimo de sacos: %d\n", sacos(pesos1, sizeof(pesos1) / sizeof(pesos1[0]), capacidade1));

    int pesos2[] = {3, 3, 3, 3, 5, 5, 11};
    int capacidade2 = 11;
    printf("Número mínimo de sacos: %d\n", sacos(pesos2, sizeof(pesos2) / sizeof(pesos2[0]), capacidade2));

    return 0;
}
