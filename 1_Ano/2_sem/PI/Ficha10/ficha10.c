#include <time.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct nodo {
    int valor;
    struct nodo *esq, *dir;
} *ABin;

ABin newABin(int r, ABin e, ABin d) {
    ABin a = malloc(sizeof(struct nodo));
    if (a != NULL) {
        a->valor = r;
        a->esq = e;
        a->dir = d;
    }
    return a;
}

ABin RandArvFromArray(int v[], int N) {
    ABin a = NULL;
    int m;
    if (N > 0) {
        m = rand() % N;
        a = newABin(v[m], RandArvFromArray(v, m), RandArvFromArray(v + m + 1, N - m - 1));
    }
    return a;
}

// Questão 1
ABin removeMenor(ABin *a) {
    if (*a == NULL) return NULL;
    if ((*a)->esq == NULL) {
        ABin temp = *a;
        *a = (*a)->dir;
        temp->dir = NULL;
        return temp;
    } else {
        return removeMenor(&((*a)->esq));
    }
}

void removeRaiz(ABin *a) {
    if ((*a)->esq == NULL && (*a)->dir == NULL) {
        free(*a);
        *a = NULL;
    } else {
        ABin temp = *a;
        if ((*a)->esq == NULL) {
            *a = (*a)->dir;
        } else if ((*a)->dir == NULL) {
            *a = (*a)->esq;
        } else {
            ABin menorDir = removeMenor(&((*a)->dir));
            menorDir->esq = (*a)->esq;
            menorDir->dir = (*a)->dir;
            free(*a);
            *a = menorDir;
        }
    }
}

int removeElem(ABin *a, int x) {
    if (*a != NULL) {
        if (x < (*a)->valor) return removeElem(&((*a)->esq), x);
        else if (x > (*a)->valor) return removeElem(&((*a)->dir), x);
        else {
            removeRaiz(a);
            return 0; //Elemento encontrado
        }
    }
    return 1; //Elemento não encontrado
}

// Questão 2
void rodaEsquerda(ABin *a) {
    ABin b = (*a)->dir;
    (*a)->dir = b->esq;
    b->esq = (*a);
    *a = b;
}

void rodaDireita(ABin *a) {
    ABin b = (*a)->esq;
    (*a)->esq = b->dir;
    b->dir = *a;
    *a = b;
}

void promoveMenor(ABin *a) {
    if ((*a) == NULL || (*a)->esq == NULL) return;
    while ((*a)->esq != NULL) rodaDireita(a);
}

void promoveMaior(ABin *a) {
    if ((*a) == NULL || (*a)->dir == NULL) return;
    while ((*a)->dir != NULL) rodaEsquerda(a);
}

ABin removeMenorAlt(ABin *a) {
    promoveMenor(a);
    removeRaiz(a);
    return *a;
}

// Questão 3
int constroiEspinhaAux(ABin *a, ABin *ult) {
    if (*a == NULL) return 0;
    int count = constroiEspinhaAux(&((*a)->esq), ult);

    if ((*a)->esq != NULL) {
        ABin temp = *ult;
        (*ult)->dir = *a;
        *ult = (*a);
        *a = (*a)->dir;
        (*ult)->dir = NULL;
        return count + 1 + constroiEspinhaAux(a, ult);
    }
    *ult = *a;
    *a = (*a)->dir;
    (*ult)->dir = NULL;
    return count + 1;
}

int constroiEspinha(ABin *a) {
    ABin ult = NULL;
    return constroiEspinhaAux(a, &ult);
}

ABin equilibraEspinha(ABin *a, int n) {
    if (n <= 0) return NULL;

    int meio = n / 2;
    ABin raiz = *a;

    for (int i = 0; i < meio; i++) {
        *a = (*a)->dir;
    }

    raiz->esq = equilibraEspinha(a, meio);
    *a = (*a)->dir;
    raiz->dir = equilibraEspinha(a, n - meio - 1);

    return raiz;
}


void equilibra(ABin *a) {
    int n = constroiEspinha(a);
    *a = equilibraEspinha(a, n);
}

void dumpABin(ABin a, int N) {
    if (a != NULL) {
        printf("%d ", a->valor);
        dumpABin(a->esq, N);
        dumpABin(a->dir, N);
    }
}

void freeABin(ABin a) {
    if (a != NULL) {
        freeABin(a->esq);
        freeABin(a->dir);
        free(a);
    }
}

int main() {
    int v1[15] = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29};
    int N = 15;

    srand(time(NULL));

    // Criando uma árvore aleatória
    ABin a1 = RandArvFromArray(v1, N);

    printf("Árvore original:\n");
    dumpABin(a1, N);
    printf("\n");

    // Removendo o menor elemento alternativamente
    printf("Removendo o menor elemento usando removeMenorAlt:\n");
    ABin menorRemovido = removeMenorAlt(&a1);
    printf("Menor elemento removido: %d\n", menorRemovido->valor);
    printf("Árvore após a remoção do menor elemento:\n");
    dumpABin(a1, --N);
    printf("\n");

    // Removendo outros elementos
    int elementoRemover = 15;
    printf("Removendo o elemento %d usando removeElem:\n", elementoRemover);
    removeElem(&a1, elementoRemover);
    printf("Árvore após a remoção do elemento %d:\n", elementoRemover);
    dumpABin(a1, --N);
    printf("\n");

    // Construindo a espinha
    printf("Construindo a espinha da árvore:\n");
    constroiEspinha(&a1);
    printf("Árvore após construção da espinha:\n");
    dumpABin(a1, N);
    printf("\n");

    // Equilibrando a espinha
    printf("Equilibrando a espinha da árvore:\n");
    equilibra(&a1);
    printf("Árvore após equilíbrio da espinha:\n");
    dumpABin(a1, N);
    printf("\n");

    // Liberando memória
    //freeABin(a1);

    return 0;
}
