#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Guiao1.h"

#define MAX_CARDS 100
#define MAX_COMBINATION_LENGTH 200

void troca(char *a, char *b) {
    char temp[5];
    memcpy(temp, a, 4);
    memcpy(a, b, 4);
    memcpy(b, temp, 4);
}

int particiona(char *cartas, int baixo, int alto) {
    int pivo = carta_para_cod(&cartas[alto * 4]);
    int i = (baixo - 1);

    for (int j = baixo; j <= alto - 1; j++) {
        if (carta_para_cod(&cartas[j * 4]) <= pivo) {
            i++;
            troca(&cartas[i * 4], &cartas[j * 4]);
        }
    }
    troca(&cartas[(i + 1) * 4], &cartas[alto * 4]);
    return (i + 1);
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
    int len = strlen(linha);

    for (int i = 0; i < len; i += 4) {
        if (strncmp(&linha[i], "\U0001F0AE", 4) == 0 || strncmp(&linha[i], "\U0001F0BE", 4) == 0 || strncmp(&linha[i], "\U0001F0DE", 4) == 0 || strncmp(&linha[i], "\U0001F0CE", 4) == 0) {
            num_reis++;
        }
    }

    return num_reis;
}

int reis(const char *comb, const char *jogadas)
{
    int contador_reis = conta_reis_na_linha(jogadas);
    int tamanho = strlen(comb) / 4;

<<<<<<< HEAD
// Função para calcular o número de combinações possíveis
int combinacoes(int n, int k) {
    int numerador = 1;
    int denominador_k = 1;
    int denominador_n_k = 1;

    // Calcula n!
    for (int i = 1; i <= n; i++) {
        numerador *= i;
    }

    // Calcula k!
    for (int i = 1; i <= k; i++) {
        denominador_k *= i;
    }

    // Calcula (n - k)!
    for (int i = 1; i <= (n - k); i++) {
        denominador_n_k *= i;
    }

    return numerador / (denominador_k * denominador_n_k);
}


int calcular_combinacoes_possiveis(const char *cartas, const char *jogada_anterior) {
    int num_combinacoes = 0;
    int len = strlen(cartas);

    // Loop para verificar todas as possíveis sub-cartas
    for (int i = 0; i < len; i += 4) {
        for (int j = i + 4; j <= len; j += 4) {
            char subcartas[j - i + 1];
            strncpy(subcartas, cartas + i, j - i);
            subcartas[j - i] = '\0';

            // Verifica o tipo de combinação da jogada anterior
            int tipo_jogada_anterior = mesmo_valor(jogada_anterior) ? 1 : (sequencia(jogada_anterior) ? 2 : (dupla_sequencia(jogada_anterior) ? 3 : 0));
            
            // Verifica o tipo de combinação da sub-lista de cartas
            int tipo_combinacao = mesmo_valor(subcartas) ? 1 : (sequencia(subcartas) ? 2 : (dupla_sequencia(subcartas) ? 3 : 0));

            // Verifica se a combinação atual é válida em relação à jogada anterior
            if (tipo_jogada_anterior == tipo_combinacao || tipo_jogada_anterior == 0) {
                num_combinacoes++;
            }
                printf("%d",num_combinacoes);

        }
    }
    return num_combinacoes;
}

int main() {
    // Exemplo de mão e jogada anterior usando emojis para representar cartas
    char mao[] = "🃆🃄🂢🃓🂵🂶🃅🃔🃖🂦🂴🂥🃒🂤🃕🃂";
    char jogada_anterior[] = "🃃🃓🂣"; // Supondo que a jogada anterior é uma sequência com 4 cartas

    // Calcula o número de combinações possíveis
    int num_combinacoes = calcular_combinacoes_possiveis(mao, jogada_anterior);
    // Imprime o resultado
    printf("Número de combinações possíveis: %d\n", num_combinacoes);
=======
    if (contador_reis == 1 && mesmo_valor(comb) && (tamanho == 4))
    {
        return 1;
    }
    if (contador_reis == 1 && dupla_sequencia(comb) && ((tamanho / 2) == 3))
    {
        return 1;
    }
    if (contador_reis == 2 && dupla_sequencia(comb) && ((tamanho / 2) == 4))
    {
        return 2;
    }
    if (contador_reis == 3 && dupla_sequencia(comb) && ((tamanho / 2) == 5))
    {
        return 3;
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
        if (valorMao >= valorUltimaJogada)
            return 1;
    }

    return 0;
}

int combinacao_valida(const char *combinacao, const char *jogadas_anteriores, int contador_reis) {
    if (contador_reis != 0) {
        if (reis(combinacao, jogadas_anteriores) != 0 || mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores)) {
            return 1;
        } else {
            return 0;
        }
    } else {
        if (mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores)) {
            return 1;
        } else if (strcmp(combinacao, "PASSO") == 0) {
            return 1;
        }
    }
    return 0;
}

void imprimir_combinacao(char *combinacao) {
    int len = strlen(combinacao);
    for (int j = 0; j < len; j += 4) {
        printf("%c%c%c%c", combinacao[j], combinacao[j+1], combinacao[j+2], combinacao[j+3]);
        if (j + 4 < len) {
            printf(" ");
        }
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
        strncpy(&comb_atual[strlen(comb_atual)], &conjunto[i], 4);
        gerar_combinacoes(conjunto, comb_atual, i + 4, tamanho, num_cartas - 1, jogada_anterior, encontrou_combinacao_valida);
        comb_atual[strlen(comb_atual) - 4] = '\0';
    }
}

void todas_combinacoes(const char *conjunto, int tamanho, const char *jogada_anterior) {
    int num_cartas_max = tamanho / 4;
    int encontrou_combinacao_valida = 0;

    for (int num_cartas = 1; num_cartas <= num_cartas_max; num_cartas++) {
        char comb_atual[MAX_COMBINATION_LENGTH] = "";
        gerar_combinacoes(conjunto, comb_atual, 0, tamanho, num_cartas, jogada_anterior, &encontrou_combinacao_valida);
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
            printf("Erro de leitura!\n");
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
        printf("Erro de leitura!\n");
        exit(1);
    }
    processar_jogadas(num_jogadas);
>>>>>>> 4a06c5ee36521e525302221402e645b8adf74cf3

    return 0;
}
