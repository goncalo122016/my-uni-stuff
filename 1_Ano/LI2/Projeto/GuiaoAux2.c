#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include "Guiao1.h"

#define MAX_CARDS 100

void cortar_cartas_menores(const char *cartas, const char *carta_referencia, char *cartas_cortadas) {
    int num_cartas = strlen(cartas) / 4;
    int carta_referencia_cod = menorMultiploDeQuatro(carta_para_cod(carta_referencia));
    const char *ptr = cartas;
    char *novo_ptr = cartas_cortadas;

    for (int i = 0; i < num_cartas; i++) {
        int codigo_carta = menorMultiploDeQuatro(carta_para_cod(ptr));
        if (codigo_carta >= carta_referencia_cod) {
            memcpy(novo_ptr, ptr, 4);
            novo_ptr += 4;
        }
        ptr += 4;
    }
    *novo_ptr = '\0';
}

void troca(char *a, char *b) {
    for (int i = 0; i < 4; i++) {
        char temp = a[i];
        a[i] = b[i];
        b[i] = temp;
    }
}

int particiona(char *cartas, int baixo, int alto) {
    int pivo = carta_para_cod(&cartas[alto * 4]);
    int i = baixo - 1;
    for (int j = baixo; j < alto; j++) {
        if (carta_para_cod(&cartas[j * 4]) <= pivo) {
            i++;
            troca(&cartas[i * 4], &cartas[j * 4]);
        }
    }
    troca(&cartas[(i + 1) * 4], &cartas[alto * 4]);
    return i + 1;
}

void quicksort(char *cartas, int baixo, int alto) {
    if (baixo < alto) {
        int pi = particiona(cartas, baixo, alto);
        quicksort(cartas, baixo, pi - 1);
        quicksort(cartas, pi + 1, alto);
    }
}

void ordena(char *cartas, int num_cartas) {
    quicksort(cartas, 0, num_cartas - 1);
}

int conta_reis_na_linha(const char *linha) {
    int num_reis = 0;
    for (const char *ptr = linha; *ptr; ptr += 4) {
        if ((*ptr == '\xF0' && *(ptr + 1) == '\x9F' && *(ptr + 2) == '\x83') &&
            (*(ptr + 3) == '\xAE' || *(ptr + 3) == '\xBE' || *(ptr + 3) == '\xDE' || *(ptr + 3) == '\xCE')) {
            num_reis++;
        }
    }
    return num_reis;
}

int reis(const char *comb, const char *jogadas) {
    int contador_reis = conta_reis_na_linha(jogadas);
    int tamanho = strlen(comb) / 4;

    if ((contador_reis == 1 && mesmo_valor(comb) && tamanho == 4) ||
        (contador_reis == 1 && dupla_sequencia(comb) && (tamanho / 2) == 3) ||
        (contador_reis == 2 && dupla_sequencia(comb) && (tamanho / 2) == 4) ||
        (contador_reis == 3 && dupla_sequencia(comb) && (tamanho / 2) == 5)) {
        return contador_reis;
    }

    return 0;
}

int mesmo_tipo_e_tamanho(const char *comb, const char *jogada_ant) {
    if (strcmp(comb, "PASSO") == 0) return 1;

    int tam_comb1 = strlen(comb);
    int tam_comb2 = strlen(jogada_ant);

    if (tam_comb1 != tam_comb2) return 0;

    int valorMao = carta_para_cod(carta_mais_alta(comb));
    int valorUltimaJogada = carta_para_cod(carta_mais_alta(jogada_ant));

    if ((sequencia(comb) && sequencia(jogada_ant)) ||
        (dupla_sequencia(comb) && dupla_sequencia(jogada_ant)) ||
        (mesmo_valor(comb) && mesmo_valor(jogada_ant))) {
        return valorMao >= valorUltimaJogada;
    }

    return 0;
}

int combinacao_valida(const char *combinacao, const char *jogadas_anteriores, int contador_reis) {
    return (contador_reis ? reis(combinacao, jogadas_anteriores) != 0 : mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores)) || strcmp(combinacao, "PASSO") == 0;
}

void imprimir_combinacao(const char *combinacao) {
    for (const char *ptr = combinacao; *ptr; ptr += 4) {
        printf("%c%c%c%c", ptr[0], ptr[1], ptr[2], ptr[3]);
        if (*(ptr + 4)) printf(" ");
    }
    printf("\n");
}

void gerar_combinacoes(const char *conjunto, char *comb_atual, int inicio, int tamanho, int num_cartas, const char *jogada_anterior, int *encontrou_combinacao_valida) {
    if (num_cartas == 0) {
        if (combinacao_valida(comb_atual, jogada_anterior, conta_reis_na_linha(jogada_anterior))) {
            imprimir_combinacao(comb_atual);
            *encontrou_combinacao_valida = 1;
        }
        return;
    }

    for (int i = inicio; i <= tamanho - 4 * num_cartas; i += 4) {
        memcpy(&comb_atual[strlen(comb_atual)], &conjunto[i], 4);
        gerar_combinacoes(conjunto, comb_atual, i + 4, tamanho, num_cartas - 1, jogada_anterior, encontrou_combinacao_valida);
        comb_atual[strlen(comb_atual) - 4] = '\0';
    }
}

void todas_combinacoes(const char *conjunto, int tamanho, const char *jogada_anterior) {
    int num_cartas_max = tamanho / 4;
    int encontrou_combinacao_valida = 0;
    char carta_referencia[5];
    
    if (strcmp("🂮", jogada_anterior) == 0 || strcmp("🂾", jogada_anterior) == 0 || strcmp("🃎", jogada_anterior) == 0 || strcmp("🃞", jogada_anterior) == 0) {
        // Se a última jogada for um dos quatro reis, não precisamos definir a carta de referência
        // Cortamos as cartas menores em relação ao próprio rei
        for (int num_cartas = 1; num_cartas <= num_cartas_max; num_cartas++) {
            char comb_atual[MAX_CARDS] = "";
            gerar_combinacoes(conjunto, comb_atual, 0, tamanho, num_cartas, jogada_anterior, &encontrou_combinacao_valida);
        }
    } else {
        // Se a última jogada não for um rei, usamos a carta mais alta da última jogada como referência
        carta_referencia[0] = jogada_anterior[0];
        carta_referencia[1] = jogada_anterior[1];
        carta_referencia[2] = jogada_anterior[2];
        carta_referencia[3] = jogada_anterior[3];
        carta_referencia[4] = '\0'; // Adicionamos o terminador nulo manualmente

        char cartas_cortadas[MAX_CARDS];
        cortar_cartas_menores(conjunto, carta_referencia, cartas_cortadas);

        for (int num_cartas = 1; num_cartas <= num_cartas_max; num_cartas++) {
            char comb_atual[MAX_CARDS] = "";
            gerar_combinacoes(cartas_cortadas, comb_atual, 0, strlen(cartas_cortadas), num_cartas, jogada_anterior, &encontrou_combinacao_valida);
        }
    }

    if (!encontrou_combinacao_valida) {
        printf("PASSO\n");
    }
}



void processar_jogadas(int num_jogadas) {
    for (int i = 0; i < num_jogadas; i++) {
        char jogada_anterior[MAX_CARDS];
        char mao[MAX_CARDS];

        if (scanf("%s %s", jogada_anterior, mao) != 2) {
            fprintf(stderr, "Erro de leitura!\n");
            exit(1);
        }

        ordena(mao, strlen(mao) / 4);
        ordena(jogada_anterior, strlen(jogada_anterior) / 4);

        printf("Teste %d\n", i + 1);
        todas_combinacoes(mao, strlen(mao), jogada_anterior);
    }
}

int main() {
    int num_jogadas;
    if (scanf("%d", &num_jogadas) != 1) {
        fprintf(stderr, "Erro de leitura!\n");
        exit(1);
    }
    processar_jogadas(num_jogadas);

    return 0;
}
