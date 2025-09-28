#include <stdio.h>
#include <wchar.h>
#include <locale.h>
#include <math.h>
#include <stdlib.h>

// Função para dar o índice onde a carta aparece no array
int carta_para_cod(wchar_t carta) {
    wchar_t baralho[] = L"🂡🂱🃁🃑🂢🂲🃂🃒🂣🂳🃃🃓🂤🂴🃄🃔🂥🂵🃅🃕🂦🂶🃆🃖🂧🂷🃇🃗🂨🂸🃈🃘🂩🂹🃉🃙🂪🂺🃊🃚🂫🂻🃋🃛🂬🂼🃌🃜🂭🂽🃍🃝🂮🂾🃎🃞";
    int num_cartas = wcslen(baralho);

    // Cada carta é representada por 1 caractere
    for (int i = 0; i < num_cartas; i++) {
        if (carta == baralho[i]) {
            return i + 1;
        }
    }
    return -1;
}

// Função para ordenar as cartas
void ordena(wchar_t *cartas) {
    int num_cartas = wcslen(cartas); // Número total de cartas
    wchar_t aux; // Variável temporária para armazenar uma carta durante a troca

    // Recorrendo ao bubble sort
    for (int i = 0; i < num_cartas - 1; i++) {
        for (int j = 0; j < num_cartas - i - 1; j++) {
            // Comparamos os valores das cartas com a função carta_para_cod
            if (carta_para_cod(cartas[j]) > carta_para_cod(cartas[j + 1])) {
                // Se a carta atual for maior que a próxima, trocamos as cartas
                aux = cartas[j];
                cartas[j] = cartas[j + 1];
                cartas[j + 1] = aux;
            }
        }
    }
}

// Função de comparação para qsort com base na última carta
int compara1(const void *a, const void *b) {
    const wchar_t *linha_a = *(const wchar_t **)a;
    const wchar_t *linha_b = *(const wchar_t **)b;
    
    int len_a = wcslen(linha_a);
    int len_b = wcslen(linha_b);
    
    const wchar_t *ultima_carta_a = &linha_a[len_a - 1];
    const wchar_t *ultima_carta_b = &linha_b[len_b - 1];
    
    return carta_para_cod(*ultima_carta_a) - carta_para_cod(*ultima_carta_b);
}

// Função para ordenar as cartas pelo código
void ordenar_linhas(wchar_t **cartas, int num_linhas) {
    qsort(cartas, num_linhas, sizeof(wchar_t *), compara1);
}

// Função para ordenar as sequências de cartas por linhas
void ordena_sequencias(wchar_t **sequencias, int num_sequencias) {
    ordenar_linhas(sequencias, num_sequencias);
}

int main() {
    setlocale(LC_ALL, ""); // Configura a localização para lidar com wide characters

    // Teste da função de ordenação de cartas
    wchar_t sequencia[] = L"🃃🂦🂪🃛🃌"; // Sequência de teste com emojis de cartas
    wprintf(L"Sequência original: %ls\n", sequencia);
    ordena(sequencia); // Ordena a sequência
    wprintf(L"Sequência ordenada: %ls\n", sequencia);

    // Teste da função de comparação para qsort
    wchar_t *test_sequences[] = {L"🃃🂦🂪🃛🃌", L"🃌🂦🂪🃛🃃", L"🃛🂦🂪🃌🃃"}; // Sequências de teste
    int num_sequences = sizeof(test_sequences) / sizeof(test_sequences[0]); // Número de sequências

    // Ordena as sequências
    for (int i=0; i< num_sequences;i++) {
    ordena_sequencias(&test_sequences[i], 1);
    }
    // Imprime as sequências ordenadas
    wprintf(L"Sequências ordenadas:\n");
    for (int i = 0; i < num_sequences; i++) {
        wprintf(L"%ls\n", test_sequences[i]);
    }

    return 0;
}
