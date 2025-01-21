#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
    float teste, minis;
} Aluno;

typedef struct lint_nodo {
    int valor;
    struct lint_nodo *prox;
} *LInt;

typedef struct abin_nodo {
    int valor;
    struct abin_nodo *esq, *dir;
} *ABin;

int isFib(int x) {
    if (x == 0 || x == 1) return 1;
    int a = 1, b = 1;
    while ((a+b) != x && (a+b) < x) {
        int s = a+b;
        a = b;
        b = s;
    }
    if ((a+b) == x) return 1;
    return 0;
}

int notaOuReprovado(Aluno a) {
    double nota = 0.8 * a.teste + 0.2 * a.minis;
    if (nota < 9.5) return 0;
    else return ((int)nota);
}

int moda (Aluno turma[], int N) {
    int freq[21] = {0}, moda = 0, maxFreq = 0;
    for (int i = 0; i < N; i++) {
        int nota = notaOuReprovado(turma[i]);
        freq[nota]++;
    }
    for (int i = 0; i < 21; i++) {
        if (freq[i] > maxFreq) {
            maxFreq = freq[i];
            moda = i;
        }
    }
    return moda;
}

int freeLInt (LInt l) {
    int lib = 0;
    while (l != NULL) {
        LInt temp = l;
        l = l->prox;
        free(temp);
        lib++;
    }
    return lib;
}

int take(int n, LInt *l) {
    if (*l == NULL) return 0;
    LInt atual, ant = NULL;
    for (atual = *l; atual != NULL && n > 0; atual = atual->prox) {
        ant = atual;
        n--;
    }
    if (ant != NULL) ant->prox = NULL;
    return freeLInt(atual);
}

int verifica(char frase[], int k) {
    int letras = 1;
    for (int i = 0; frase[i] != '\0'; i++) {
        if (frase[i] != ' ') letras++;
        else{
            if (letras < k) return 0;
            else letras = 1;
        }
    }
    return 1;
}

void impLI (LInt l) {
    while (l != NULL) {
        printf("%d ", l->valor);
        l = l->prox;
    }
    printf("\n");
}

ABin reconstroiAux(char **s) {
    if (**s == '*') {
        (*s)++;
        return NULL;
    }
    ABin a = malloc(sizeof(struct abin_nodo));
    a->valor = **s - '0';
    (*s)++;
    a->esq = reconstroiAux(s);
    a->dir = reconstroiAux(s);
    return a;
}

ABin reconstroi(char s[]) {
    return reconstroiAux(&s);
}

// Function to print the tree (pre-order traversal)
void printTree(ABin a) {
    if (a == NULL) {
        printf("*");
        return;
    }
    printf("%d", a->valor);
    printTree(a->esq);
    printTree(a->dir);
}

int main() {
    // Teste para isFib
    int fibTest = 21;
    if (isFib(fibTest)) {
        printf("%d is a Fibonacci number.\n", fibTest);
    } else {
        printf("%d is not a Fibonacci number.\n", fibTest);
    }

    // Teste para moda
    Aluno turma[] = {
        {15.0, 18.0}, {10.0, 10.0}, {15.0, 18.0}, {10.0, 10.0}, {9.0, 10.0}
    };
    int N = 5;
    for (int i = 0; i<N; i++) printf("%d ", notaOuReprovado(turma[i]));
    printf("\n");
    int nota_moda = moda(turma, N);
    printf("The most frequent final grade is: %d\n", nota_moda);

    // Teste para take
    LInt l = (LInt) malloc(sizeof(struct lint_nodo));
    l->valor = 1;
    l->prox = (LInt) malloc(sizeof(struct lint_nodo));
    l->prox->valor = 2;
    l->prox->prox = (LInt) malloc(sizeof(struct lint_nodo));
    l->prox->prox->valor = 3;
    l->prox->prox->prox = NULL;
    impLI(l);
    int removed = take(2, &l);
    printf("Number of nodes removed: %d\n", removed);
    impLI(l);

    // Teste para verifica
    char frase[] = "Hello world this is a test";
    int k = 1;
    if (verifica(frase, k)) {
        printf("All words have at least %d characters.\n", k);
    } else {
        printf("Not all words have at least %d characters.\n", k);
    }

    // Teste para reconstroi
    char s[] = "34**52**5*6**";
    ABin a = reconstroi(s);
    
    printTree(a);
    printf("\n");

    return 0;
}