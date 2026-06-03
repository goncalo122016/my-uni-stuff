#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Definição do tipo de dado para a lista encadeada
typedef struct LInt_nodo {
    int valor;
    struct LInt_nodo *prox;
} *LInt;

// Definição do tipo de dado para a árvore binária
typedef struct ABin_nodo {
    int valor;
    struct ABin_nodo *esq, *dir;
} *ABin;

// Protótipos das funções
int pesquisa(int a[], int N, int x){
    int i;
    for(i = 0; i < N && a[i] < x; i++){
        if(a[i] == x){
            return i;
        }
    }
    return -1;
}

void roda(LInt *l) {
    if ((*l) == NULL || (*l)->prox == NULL) return;
    LInt nova = *l;
    LInt atual, ant;
    for (atual = (*l); atual->prox != NULL; atual = atual->prox) {
        ant = atual;
    }
    ant->prox = NULL;
    atual->prox = nova;
    *l = atual;
}

int apaga(ABin *a, int n) {
    if (*a == NULL || n <= 0) {
        return 0;
    }

    int apagados_esq = apaga(&((*a)->esq), n);
    if (apagados_esq >= n) {
        return apagados_esq;
    }

    int apagados_dir = apaga(&((*a)->dir), n - apagados_esq);
    int total_apagados = apagados_esq + apagados_dir + 1;

    free(*a);
    *a = NULL;

    return total_apagados;
}

int somaDigitos(int n) {
    int soma = 0;
    for (n; n>0; n/=10){
        soma += n % 10;
    }
    return soma;
}

void checksum(char s[]){
    int contaSoma = 0, len = strlen(s);
    for (int i = 0; s[i]; i++) {
        if ((len - i) % 2 == 0) contaSoma += (s[i] - '0');
        else {
            int valorIpar = (s[i] - '0') * 2;
            contaSoma += somaDigitos (valorIpar);
        }
    }
    s[len] = '0' + (contaSoma % 10);
    s[len + 1] = '\0';
}

int max(int a, int b) {
    return (a > b) ? a : b;
}

int escolhe(int N, int valor[], int peso[], int C, int quant[]) {
    int dp[C+1]; // Tabela de programação dinâmica para armazenar os valores máximos
    int escolha[C+1][N]; // Tabela para armazenar as escolhas feitas

    // Inicializa a primeira linha da tabela dp com zeros
    for (int i = 0; i <= C; i++) {
        dp[i] = 0;
    }

    // Inicializa a tabela de escolhas com zeros
    for (int i = 0; i <= C; i++) {
        for (int j = 0; j < N; j++) {
            escolha[i][j] = 0;
        }
    }

    // Preenche a tabela de programação dinâmica
    for (int i = 0; i <= C; i++) {
        for (int j = 0; j < N; j++) {
            if (peso[j] <= i) {
                if (dp[i] < dp[i - peso[j]] + valor[j]) {
                    dp[i] = dp[i - peso[j]] + valor[j];
                    for (int k = 0; k < N; k++) {
                        escolha[i][k] = escolha[i - peso[j]][k];
                    }
                    escolha[i][j]++;
                }
            }
        }
    }

    // Copia as escolhas para o array quant
    for (int i = 0; i < N; i++) {
        quant[i] = escolha[C][i];
    }

    // Retorna o valor máximo obtido
    return dp[C];
}

// Função para criar um novo nodo para a árvore binária
ABin novoNodo(int valor) {
    ABin novo = malloc (sizeof(struct ABin_nodo));
    novo->valor = valor;
    novo->esq = NULL;
    novo->dir = NULL;
}

void imprimeABin (ABin a) {
    if (a == NULL) {
        return;
    }
    printf("%d ", a->valor);
    imprimeABin(a->esq);
    imprimeABin(a->dir);
}

int main() {
    // Testes para a função pesquisa
    int array_pesquisa[] = {1, 3, 5, 7, 9, 11, 13};
    int indice = pesquisa(array_pesquisa, sizeof(array_pesquisa) / sizeof(array_pesquisa[0]), 9);
    printf("Pesquisa: Índice de 7: %d\n", indice); // Deverá ser 3

    // Testes para a função roda
    LInt lista = malloc(sizeof(struct LInt_nodo));
    lista->valor = 1;
    lista->prox = malloc(sizeof(struct LInt_nodo));
    lista->prox->valor = 2;
    lista->prox->prox = malloc(sizeof(struct LInt_nodo));
    lista->prox->prox->valor = 3;
    lista->prox->prox->prox = NULL;

    roda(&lista);
    printf("Roda: %d %d %d\n", lista->valor, lista->prox->valor, lista->prox->prox->valor); // Deverá ser 3 1 2

    // Testes para a função apaga
    ABin raiz = novoNodo(1);
    raiz->esq = novoNodo(2);
    raiz->dir = novoNodo(3);
    raiz->esq->esq = novoNodo(4);
    raiz->esq->dir = novoNodo(5);
    raiz->dir->esq = novoNodo(6);
    raiz->dir->dir = novoNodo(7);
    printf("Árvore Orginal:\n");
    imprimeABin(raiz);
    printf("\n");

    // Testando a função apaga
    int nodos_apagados = apaga(&raiz, 3);
    printf("Nodos apagados: %d\n", nodos_apagados);
    imprimeABin(raiz);
    printf("\n");

    // Testes para a função checksum
    char identificador1[6] = "9871";
    //printf ("Digitos: %d\n", somaDigitos(1));
    checksum(identificador1);
    printf("Checksum de '9871': %s\n", identificador1); // Deverá ser "98715"

    // Testes para a função escolhe
    int N = 3;
    int valor[] = {20, 150, 30};
    int peso[] = {2, 10, 3};
    int C = 14;
    int quant[N];

    // Chama a função para determinar a quantidade de produtos a transportar
    int valorTotal = escolhe(N, valor, peso, C, quant);

    // Exibe os resultados
    printf("Quantidades escolhidas:\n");
    for (int i = 0; i < N; i++) {
        printf("Produto %d: %d\n", i+1, quant[i]);
    }
    printf("Valor total dos produtos escolhidos: %d\n", valorTotal);

    return 0;
}
