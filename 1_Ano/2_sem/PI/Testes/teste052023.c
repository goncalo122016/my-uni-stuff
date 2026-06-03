#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

// Estrutura de um Ponto
typedef struct {
    int x, y;
} Ponto;

// Estrutura de um nodo de uma árvore binária
typedef struct abin_nodo {
    int valor;
    struct abin_nodo *esq, *dir;
} *ABin;

// Estrutura de um nodo de uma lista ligada
typedef struct lint_nodo {
    int valor;
    struct lint_nodo *prox;
} *LInt;

// Exercício 1: Testa se um número é perfeito
int perfeito(int x) {
    if (x == 0) return 0; 
    int i, soma = 0;
    for (i = 1; i < x; i++) {
        if (x % i == 0) soma += i;
    }
    if (soma == x) return 1;
    else return 0;
}

// Exercício 2: Ordena pontos por distância à origem
void swap(Ponto pos[], int i, int j) {
    Ponto aux = pos[i];
    pos[i] = pos[j];
    pos[j] = aux;
}

int distOrg (Ponto p) {
    return sqrt(p.x * p.x + p.y * p.y);
}

void ordena(Ponto pos[], int N) {
    int i, j;
    for (i = N; i > 0; i--){
        for (j = 1; j<i; j++){
            if (distOrg(pos[j-1]) > distOrg(pos[j])) swap(pos, j-1, j);
        }
    }
}

// Exercício 3: Retorna o menor nível em que um elemento se encontra na árvore
int depth(ABin a, int x) {
    if (a == NULL) return (-1);
    if (a->valor == x) return 0;
    int menor_esq = depth(a->esq, x);
    int menor_dir = depth(a->dir, x);

    if (menor_dir == -1 && menor_esq == -1) return (-1);
    if (menor_esq == -1) return menor_dir + 1;
    if (menor_dir == -1) return menor_esq + 1;

    if (menor_dir < menor_esq) return 1 + menor_dir;
    else return 1 + menor_esq;
}

// Exercício 4: Verifica a pontuação e modifica a tentativa
int wordle(char secreta[], char tentativa[]) {
    int r = 0;
    for (int i = 0; secreta[i]; i++) {
        if (secreta[i] == tentativa[i]) {
            tentativa[i] = toupper(tentativa[i]);
            r++;
        } else {
            if (strchr(secreta, tentativa[i]) == NULL) {
                tentativa[i] = '*';
            }
        }    
    }
    return r;
}

// Exercício 5: Constrói uma lista circular com uma sequência infinita periódica de dígitos
LInt periodica(char s[]) {
    LInt inicio = NULL;
    LInt atual = NULL;
    int len = strlen(s);
    int i = 0;

    while (s[i] != '(' && s[i] != '\0') {
        if (isdigit(s[i])) {
            LInt novo = malloc(sizeof(struct lint_nodo));
            novo->valor = s[i] - '0';
            novo->prox = NULL;
            if (inicio == NULL) {
                inicio = novo;
                atual = inicio;
            } else {
                atual->prox = novo;
                atual = atual->prox;
            }
        }
        i++;
    }

    if (s[i] == '(') {
        int j = i + 1;
        while (s[j] != ')' && s[j] != '\0') {
            if (isdigit(s[j])) {
                LInt novo = malloc(sizeof(struct lint_nodo));
                novo->valor = s[j] - '0';
                novo->prox = NULL;
                atual->prox = novo;
                atual = atual->prox;
            }
            j++;
        }
        atual->prox = inicio;
    }

    return inicio;
}

void imprimirListaCircular(LInt lista, int maxIteracoes) {
    if (lista == NULL || maxIteracoes == 0) {
        printf("Lista vazia ou número máximo de iterações atingido.\n");
        return;
    }
    LInt atual = lista;
    printf("%d", atual->valor);
    atual = atual->prox;
    while (atual != lista && maxIteracoes > 1) {
        printf(" -> %d", atual->valor);
        atual = atual->prox;
        maxIteracoes--;
    }
    printf(" -> ...\n");
}

int main() {
    // Exercício 1
    int num_perfeito = 28;
    printf("Número %d é%s perfeito.\n", num_perfeito, perfeito(num_perfeito) ? "" : " não");

    // Exercício 2
    Ponto pontos[] = {{3, 3}, {2, 1}, {-1, 0}};
    int num_pontos = sizeof(pontos) / sizeof(pontos[0]);
    ordena(pontos, num_pontos);
    printf("Pontos ordenados por distância à origem:\n");
    for (int i = 0; i < num_pontos; i++) {
        printf("(%d, %d) ", pontos[i].x, pontos[i].y);
    }
    printf("\n");

    // Exercício 3
    ABin arvore = malloc(sizeof(struct abin_nodo));
    arvore->valor = 5;
    arvore->esq = malloc(sizeof(struct abin_nodo));
    arvore->esq->valor = 3;
    arvore->esq->esq = NULL;
    arvore->esq->dir = NULL;
    arvore->dir = malloc(sizeof(struct abin_nodo));
    arvore->dir->valor = 8;
    arvore->dir->esq = NULL;
    arvore->dir->dir = NULL;
    int elemento = 3;
    printf("Profundidade do elemento %d na árvore: %d\n", elemento, depth(arvore, elemento));

    // Exercício 4
    char secreta[] = "laranja";
    char tentativa1[] = "cerejas";
    char tentativa2[] = "bananas";
    int score1 = wordle(secreta, tentativa1);
    int score2 = wordle(secreta, tentativa2);
    printf("Pontuação tentativa 1: %d\n", score1);
    printf("Tentativa 1 modificada: %s\n", tentativa1);
    printf("Pontuação tentativa 2: %d\n", score2);
    printf("Tentativa 2 modificada: %s\n", tentativa2);

    // Exercício 5
    char sequencia[] = "34(56)";
    LInt lista = periodica(sequencia);
    printf("Lista circular construída com a sequência periódica:\n");
    LInt temp = lista;
    imprimirListaCircular(temp, 10);
    printf("\n");

    return 0;
}