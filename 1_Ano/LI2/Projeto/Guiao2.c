#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <locale.h>
#include <string.h>
#include "teste.h"

// Função para ordenar as cartas
void ordena(char *cartas) {
    int num_cartas = strlen(cartas) / 4; // Número total de cartas (cada carta é constituida por 4 caracteres)
    char aux[5]; // Array temporário para armazenar uma carta durante a troca

    // Recorrendo ao bubble sort (PowerPoints PI)
    for (int i = 0; i < num_cartas - 1; i++) {
        for (int j = 0; j < num_cartas - i - 1; j++) {
            // Comparamos os valores das cartas com a função carta_para_cod
            if (carta_para_cod(&cartas[j * 4]) > carta_para_cod(&cartas[(j + 1) * 4])) {
                // Se a carta atual for maior que a próxima, trocamos as cartas
                memcpy(aux, &cartas[j * 4], 4);
                memcpy(&cartas[j * 4], &cartas[(j + 1) * 4], 4);
                memcpy(&cartas[(j + 1) * 4], aux, 4);
            } 
        }
    }
}

// Função de comparação para qsort com base na última carta
int compara1(const void *a, const void *b) {
    const char *linha_a = *(const char **)a;
    const char *linha_b = *(const char **)b;
    
    int len_a = strlen(linha_a);
    int len_b = strlen(linha_b);
    
    const char *ultima_carta_a = &linha_a[len_a - 4];
    const char *ultima_carta_b = &linha_b[len_b - 4];
    
    return carta_para_cod(ultima_carta_a) - carta_para_cod(ultima_carta_b);
}

// Função para ordenar as cartas pelo código, considerando que o número de cartas é pequeno e o desempenho não é crítico 
void ordenar_linhas(char **cartas, int num_linhas) {
    qsort(cartas, num_linhas, sizeof(char *), compara1);
}

// Função para ordenar as sequências de cartas por linhas (após a ordenação de cada linha) 
void ordena_sequencias(char **sequencias, int num_sequencias) {
    ordenar_linhas(sequencias, num_sequencias); // Ordena todas as sequências
}


int verifica_sequencias(const char * const *sequencias, int num_sequencias) {
    if (num_sequencias == 0) return 0;

    int tam_prim_seq = strlen(sequencias[0]) / 4; // Comprimento da primeira sequência em número de cartas

    for (int i = 1; i < num_sequencias; i++) {
        int tam_atual = strlen(sequencias[i]) / 4;
        if (tam_atual != tam_prim_seq) {
            return 0; // Retorna 0 se encontrarmos uma sequência com um comprimento diferente
        }
        if (sequencia(sequencias[i]) && sequencia(sequencias[0])) {
            continue;
        } else if (dupla_sequencia(sequencias[i]) && dupla_sequencia(sequencias[0])) {
            continue;
        } else if (mesmo_valor(sequencias[i]) && mesmo_valor(sequencias[0])) {
            continue;
        } {
            return 0; // Retorna 0 se a sequência não for do tipo esperado
        }
    }

    return 1; // Todas as sequências têm o mesmo valor e são do tipo esperado
}
// Função para ler as sequências de cartas
char** ler_sequencias(int num_sequencias) {
    char **sequencias = (char **)malloc(num_sequencias * sizeof(char *));
    if (!sequencias) {
        printf("Erro ao alocar memória.\n");
        exit(1);
    }

    for (int i = 0; i < num_sequencias; i++) {
        sequencias[i] = (char *)malloc(101 * sizeof(char));
        if (!sequencias[i]) {
            printf("Erro ao alocar memória.\n");
            exit(1);
        }
        if (scanf("%s", sequencias[i]) != 1) {
            printf("Erro ao ler as sequências de cartas.\n");
            exit(1);//Uma vez que estamos a dar return a um char** não podemos dar return de um int  
        }
        ordena(sequencias[i]); // Ordena cada sequência (função definida anteriormente)
    }

    return sequencias;
}

// Função para imprimir as sequências de cartas ordenadas
void imprimir_sequencias(char **sequencias, int num_sequencias, int teste) {
    if (verifica_sequencias((const char **)sequencias, num_sequencias)) {
        printf("Teste %d\n", teste);
        for (int i = 0; i < num_sequencias; i++) {
            int len = strlen(sequencias[i]);
            for (int j = 0; j < len; j += 4) {
                printf("%c%c%c%c", sequencias[i][j], sequencias[i][j+1], sequencias[i][j+2], sequencias[i][j+3]); //  imprime uma carta, que consiste em 4 caracteres, na mesma linha.
                if (j < len - 4) {
                    printf(" ");
                }
            }
            printf("\n"); // Imprime uma nova linha após imprimir todas as cartas de uma sequência
        }
    } else {
        printf("Teste %d\nCombinações não iguais!\n", teste);
    } 
}

// Função para liberar a memória alocada para as sequências (anteriormente alocada pelo malloc)
void liberar_sequencias(char **sequencias, int num_sequencias) {
    for (int i = 0; i < num_sequencias; i++) {
        free(sequencias[i]);
    }
    free(sequencias);
}

/*
int main() {
    int num_testes;
    if (scanf("%d", &num_testes) != 1) {
        printf("Erro ao ler o número de expressões.\n");
        return 1;
    }
  
    for (int t = 1; t <= num_testes; t++) {
        int num_sequencias;
        if (scanf("%d", &num_sequencias) != 1) {
            printf ("Erro ao ler o número de expressões.\n");
            return 1;
        }

        // Ler as sequências de cartas
        char **sequencias = ler_sequencias(num_sequencias);

        // Ordenar as sequências de cartas
        ordena_sequencias(sequencias, num_sequencias);

        // Imprimir as sequências ordenadas
        imprimir_sequencias(sequencias, num_sequencias, t);

        // Liberar a memória alocada para as sequências
        liberar_sequencias(sequencias, num_sequencias);
    }

    return 0;
}
*/
