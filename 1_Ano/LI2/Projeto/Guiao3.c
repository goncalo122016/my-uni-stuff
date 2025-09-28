#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Guiao1.h"

#define MAX_CARDS 5000
#define MAX_TESTES 200

void imprimir_testes(char testes[MAX_TESTES][MAX_CARDS], int num_testes)
{
    for (int i = 0; i < num_testes; i++)
    {
        printf("Teste %d\n", i + 1);
        int primeira_carta = 1;
        for (int j = 0; testes[i][j] != '\0'; j += 4)
        {
            if (!primeira_carta)
            {
                printf(" "); // Adiciona um espaço entre as cartas, exceto a primeira
            }
            else
            {
                primeira_carta = 0;
            }
            printf("%c%c%c%c", testes[i][j], testes[i][j + 1], testes[i][j + 2], testes[i][j + 3]); // Imprime uma carta
        }
        printf("\n");
    }
}

int conta_reis_na_linha(const char *linha)
{
    int num_reis = 0;
    int len = strlen(linha);

    int all_kings = 1; // Flag para indicar se todos as cartas na linha são reis

    // Percorre a linha e conta o número de cartas REI
    for (int i = 0; i < len; i += 4)
    {
        // Compara cada carta individualmente com as cartas REI
        if (strncmp(&linha[i], "\U0001F0AE", 4) != 0 && strncmp(&linha[i], "\U0001F0BE", 4) != 0 && strncmp(&linha[i], "\U0001F0DE", 4) != 0 && strncmp(&linha[i], "\U0001F0CE", 4) != 0)
        {
            all_kings = 0; // Se encontrar uma carta que não é rei, define a flag como 0
        }
        else
        {
            num_reis++;
        }
    }

    if (all_kings)
    {
        return num_reis; // Retorna o número de reis apenas se todos as cartas forem reis
    }
    else
    {
        return 0; // Se alguma carta não for rei, retorna 0
    }
}

int conta_reis_jogadas(char **jogadas, int num_jogadas)
{
    int num_reis = 0;

    // Percorre as jogadas anteriores e conta o número de reis em cada uma
    for (int i = num_jogadas - 1; i >= num_jogadas - 3 && i >= 0; i--)
    {
        num_reis += conta_reis_na_linha(jogadas[i]);
    }

    return num_reis;
}

int reis(const char *comb, char **jogadas, int num_jogadas)
{
    int contador_reis = conta_reis_jogadas(jogadas, num_jogadas);
    int tamanho = strlen(comb) / 4;

    // Verifica se alguma das próximas três jogadas atende às condições especificadas
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


int mesmo_tipo_e_tamanho(const char *comb, const char *jogada_ant)
{
    int tam_comb1 = strlen(comb); // Obtém o tamanho da combinação 1
    int tam_comb2 = strlen(jogada_ant); // Obtém o tamanho da combinação 2
    
    if (strcmp(comb, "PASSO") == 0) return 1;
    
    // Verifica se as combinações têm o mesmo tamanho
    if (tam_comb1 != tam_comb2)
    {
        return 0;
    }

    int valorMao = carta_para_cod(carta_mais_alta(comb)) ;
    int valorUltimaJogada = carta_para_cod(carta_mais_alta(jogada_ant));

    // Verifica se as combinações têm o mesmo tipo
    
    if ((sequencia(comb) && sequencia(jogada_ant)) ||
        (dupla_sequencia(comb) && dupla_sequencia(jogada_ant)) ||
        (mesmo_valor(comb) && mesmo_valor(jogada_ant)))
    {
        if (valorMao >= valorUltimaJogada)
            return 1; // As combinações têm o mesmo tipo e a primeira é maior que a segunda
    }

    return 0; // As combinações não têm o mesmo tipo
}

void ordena(char *cartas)
{
    int num_cartas = strlen(cartas) / 4; // Número total de cartas (cada carta é constituída por 4 caracteres)
    char aux[5];                         // Array temporário para armazenar uma carta durante a troca

    // Recorrendo ao bubble sort
    for (int i = 0; i < num_cartas - 1; i++)
    {
        for (int j = 0; j < num_cartas - i - 1; j++)
        {
            // Comparamos os valores das cartas com a função carta_para_cod
            if (carta_para_cod(&cartas[j * 4]) > carta_para_cod(&cartas[(j + 1) * 4]))
            {
                // Se a carta atual for maior que a próxima, trocamos as cartas
                memcpy(aux, &cartas[j * 4], 4);
                memcpy(&cartas[j * 4], &cartas[(j + 1) * 4], 4);
                memcpy(&cartas[(j + 1) * 4], aux, 4);
            }
        }
    }
}

void remover_carta(char *mao, const char *jogada)
{
    int len = strlen(jogada);
    char *ptr = mao;
    while (*ptr != '\0')
    { // Percorre toda a mão
        int found = 0;
        for (int i = 0; i < len; i += 4)
        { // Verifica se a carta atual da mão está na jogada
            if (strncmp(ptr, jogada + i, 4) == 0)
            {
                found = 1;
                break;
            }
        }
        if (found)
        { // Se a carta está na jogada, remove ela da mão
            for (int i = 0; i < len; i += 4)
            {
                if (strncmp(ptr, jogada + i, 4) == 0)
                {
                    memmove(ptr, ptr + 4, strlen(ptr + 4) + 1); // Move todas as cartas após a encontrada
                }
            }
        }
        else
        {
            ptr += 4; // Passa para a próxima carta na mão
        }
    }
}

int seq_tres_passos(char **jogadas, int num_jogadas)
{
    int count_passo = 0;
    // Começa a partir das últimas três jogadas
    for (int i = num_jogadas - 1; i >= 0 && i >= num_jogadas - 3; i--)
    {
        if (strcmp(jogadas[i], "PASSO") == 0)
        {
            count_passo++;
            if (count_passo == 3)
            {
                return 1;
            }
        }
        else
        {
            count_passo = 0;
        }
    }
    return 0;
}

int combinacao_valida(const char *combinacao, char **jogadas_anteriores, int num_jogadas)
{
    // Verifica se ocorreu uma sequência de três passos consecutivos
    int tres_passos = seq_tres_passos(jogadas_anteriores, num_jogadas);

    // Verifica se não há jogadas anteriores
    if (num_jogadas == 0 || tres_passos == 1)
    {
        // Verifica se a combinação é um conjunto, uma sequência ou uma dupla sequência
        if (mesmo_valor(combinacao) || sequencia(combinacao) || dupla_sequencia(combinacao)) 
            return 1; // Combinação válida
        else 
            return 0; // Combinação inválida
    }

    if (conta_reis_jogadas(jogadas_anteriores, num_jogadas) != 0){
        // Verifica se a combinação possui reis
        if (reis(combinacao, jogadas_anteriores, num_jogadas) > 0) return 1; // Combinação válida
        else return 0; // Combinação inválida
    }
    
    if (num_jogadas >= 3) {
        // Verifica se a combinação atual é do mesmo tipo e tamanho que alguma das três últimas jogadas
        if (mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores[num_jogadas - 1]) ||
            mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores[num_jogadas - 2]) ||
            mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores[num_jogadas - 3]))
                return 1; // Combinação válida
    }

    else if (num_jogadas == 2) {
        // Verifica se a combinação atual é do mesmo tipo e tamanho que alguma das três últimas jogadas
        if (mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores[num_jogadas - 1]) ||
            mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores[num_jogadas - 2]))
                return 1; // Combinação válida
    }
        
    else if (num_jogadas == 1) {
        // Verifica se a combinação atual é do mesmo tipo e tamanho que alguma das três últimas jogadas
        if (mesmo_tipo_e_tamanho(combinacao, jogadas_anteriores[num_jogadas - 1]))
                return 1; // Combinação válida
    }
        
    return 0; // Combinação inválida
}

// Função para ler a mão do jogador
void ler_mao(char *mao)
{
    if (scanf("%s", mao) != 1)
    {
        printf("Erro de leitura!\n");
        exit(1);
    }

    ordena(mao); // Ordena a mão após ler a entrada do jogador
}

// Função para processar as jogadas anteriores e armazená-las
char **processar_jogadas_anteriores(int num_jogadas)
{
    char **jogadas_anteriores = malloc(num_jogadas * sizeof(char *)); // Aloca memória para o array de ponteiros

    if (jogadas_anteriores == NULL)
    { // Verifica se a alocação de memória falhou
        perror("Erro ao alocar memória");
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < num_jogadas; i++)
    {
        jogadas_anteriores[i] = malloc((MAX_CARDS + 1) * sizeof(char)); // Aloca memória para a jogada atual

        if (jogadas_anteriores[i] == NULL)
        { // Verifica se a alocação de memória falhou
            perror("Erro ao alocar memória");
            exit(EXIT_FAILURE);
        }

        // Lê a jogada atual
        if (scanf("%s", jogadas_anteriores[i]) != 1)
        {
            fprintf(stderr, "Erro ao ler a jogada %d\n", i + 1);
            exit(EXIT_FAILURE);
        }
        jogadas_anteriores[i][strlen(jogadas_anteriores[i])] = '\0'; // Garante que a string seja terminada corretamente
    }

    return jogadas_anteriores; // Retorna o array de ponteiros para as jogadas anteriores
}

// Função para realizar a jogada
void realizar_jogada(char *mao, char *jogada)
{
    int len = strlen(jogada);

    for (int i = 0; i < len; i += 4){
        remover_carta(mao, &jogada[i]);
    }
    ordena(mao); // Ordena a mão após a jogada
}


// Função para imprimir a mão atual
void imprimir_mao(char *mao, int numero_rodada)
{
    printf("Teste %d\n", numero_rodada);
    ordena(mao);
    int primeira_carta = 1;
    for (int i = 0; mao[i] != '\0'; i += 4)
    {
        if (!primeira_carta)
        {
            printf(" "); // Adiciona um espaço entre as cartas, exceto a primeira
        }
        else
        {
            primeira_carta = 0;
        }
        printf("%c%c%c%c", mao[i], mao[i + 1], mao[i + 2], mao[i + 3]); // Imprime uma carta
    }
    printf("\n");
}

// Função principal para processar uma rodada do jogo
void processar_rodada(int numero_rodada)
{
    int num_jogadas;
    if (scanf("%d", &num_jogadas) != 1)
    {
        printf("Erro de leitura!\n");
        exit(1);
    }

    char mao[MAX_CARDS];
    ler_mao(mao);

    char **jogadas_anteriores = processar_jogadas_anteriores(num_jogadas);

    char jogada[MAX_CARDS];
    if (scanf("%s", jogada) != 1)
    {
        printf("Erro de leitura!\n");
        exit(1);
    }
    if (combinacao_valida(jogada, jogadas_anteriores, num_jogadas))
    {
        realizar_jogada(mao, jogada);
        imprimir_mao(mao, numero_rodada);
    }
    else
    {
        imprimir_mao(mao, numero_rodada); // Imprime a mão original se a jogada for inválida
    }
    free(jogadas_anteriores);
}

int main()
{
    int T;
    if (scanf("%d", &T) != 1)
    {
        printf("Erro de leitura!\n");
        return 1;
    }

    char testes[MAX_TESTES][MAX_CARDS]; // Array auxiliar para armazenar as mãos de cada teste

    for (int t = 0; t < T; t++)
    {
        int num_jogadas;
        if (scanf("%d", &num_jogadas) != 1)
        {
            printf("Erro de leitura!\n");
            return 1;
        }

        char mao[MAX_CARDS];
        ler_mao(mao);

        char **jogadas_anteriores = processar_jogadas_anteriores(num_jogadas);

        char jogada[MAX_CARDS];
        if (scanf("%s", jogada) != 1)
        {
            printf("Erro de leitura!\n");
            return 1;
        }
        if (combinacao_valida(jogada, jogadas_anteriores, num_jogadas))
        {
            realizar_jogada(mao, jogada);
            strcpy(testes[t], mao); // Copia a mão atual para o array auxiliar
        }
        else
        {
            strcpy(testes[t], mao); // Se a jogada for inválida, copia a mão original
        }
        free(jogadas_anteriores);
    }

    imprimir_testes(testes, T); // Imprime todas as mãos ao final

    return 0;
}