#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int carta_para_cod(const char *carta) {
    const char *baralho = "🂡🂱🃁🃑🂢🂲🃂🃒🂣🂳🃃🃓🂤🂴🃄🃔🂥🂵🃅🃕🂦🂶🃆🃖🂧🂷🃇🃗🂨🂸🃈🃘🂩🂹🃉🃙🂪🂺🃊🃚🂫🂻🃋🃛🂬🂼🃌🃜🂭🂽🃍🃝🂮🂾🃎🃞";
    int num_cartas = strlen(baralho) / 4;

    // A função memcmp compara os primeiros 4 bytes da carta atual com os 4 primeiros bytes da carta que está sendo passada
    for (int i = 0; i < num_cartas; i++) {
        if (memcmp(carta, &baralho[i * 4], 4) == 0) {
            return i + 1;
        }
    }
    return -1;
}

void ordena(char *cartas) {
    int num_cartas = strlen(cartas) / 4; // Número total de cartas (cada carta é constituída por 4 caracteres)
    char *aux = malloc(5); // Array temporário para armazenar uma carta durante a troca

    // Recorrendo ao bubble sort
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

    free(aux);
}

// Função compara para o qsort
int compara(const void *a, const void *b) {
    return (*(int *)a - *(int *)b);
}
// Função para preencher um array com os índices das cartas
void indices_array(const char *cartas, int *resultado) {
    int indice = 0;
    int num_cartas = strlen(cartas);

    // Cada carta é representada por 4 caracteres
    for (int i = 0; i < num_cartas; i += 4) {
        resultado[indice] = carta_para_cod(&cartas[i]);
        indice++;
    }
}

// Função para dar o menor múltiplo de 4 que seja maior ou igual ao número dado (pois cada valor possui 4 naipes)
int menorMultiploDeQuatro(int valor) {
    int multiplo = valor;
    while (multiplo % 4 != 0) {
        multiplo++;
    }
    return multiplo;
}

/* Função para verificar se um conjunto de cartas é uma dupla sequência
int dupla_sequencia(const char *cartas[], int num_conjuntos) {
    int encontradas = 0;

    for (int k = 0; k < num_conjuntos; k++) {
        int num_cartas = strlen(cartas[k]) / 4;
        if (num_cartas < 6) continue;

        for (int i = 0; i < num_cartas*4; i += 4) {
            if ((carta_para_cod(&cartas[k][i * 4]) == carta_para_cod(&cartas[k][(i + 2) * 4]) - 1) &&
                (carta_para_cod(&cartas[k][(i + 2) * 4]) == carta_para_cod(&cartas[k][(i + 4) * 4]) - 1)) {
                printf("Dupla sequência encontrada: %s\n", cartas[k]);
                encontradas++;
            }
        }
    }

    return encontradas;
}*/

int verifica_par(char *cartas) {
    int num_cartas = strlen(cartas) / 4;
    if (num_cartas <= 4) return 0;

    int indices[num_cartas];
    indices_array(cartas, indices);
    qsort(indices, num_cartas, sizeof(int), compara);

    for (int i = 0; i < num_cartas ; i += 2) {
        if ((menorMultiploDeQuatro(indices[i]) == menorMultiploDeQuatro(indices[i + 1])) &&
            (menorMultiploDeQuatro(indices[i]) + 4 == menorMultiploDeQuatro(indices[i + 2]))) {
            return 1;
        }
    }

    return 0;
}

int dupla_sequencia(char *cartas) {
    int num_cartas = strlen(cartas) / 4;
    if (num_cartas <= 4) return 0;
     int i;
    int indices[num_cartas];
    indices_array(cartas, indices);
    qsort(indices, num_cartas, sizeof(int), compara);

    // Verifica se há uma sequência de cartas pares
    int sequencia_pares = 0;
    for ( i = 0; i < num_cartas ; i += 2) {
        if ((menorMultiploDeQuatro(indices[i]) == menorMultiploDeQuatro(indices[i + 1])) &&
            (menorMultiploDeQuatro(indices[i]) + 4 == menorMultiploDeQuatro(indices[i + 2]))) {
            sequencia_pares++ ;
           
        }
    }
   
    // Verifica se há uma sequência de cartas ímpares
    int sequencia_impares = 0;
    for ( i = 1; i < num_cartas ; i += 2) {
        if ((menorMultiploDeQuatro(indices[i]) == menorMultiploDeQuatro(indices[i - 1])) &&
            (menorMultiploDeQuatro(indices[i]) + 4 == menorMultiploDeQuatro(indices[i + 1]))) {
            sequencia_impares++;
         

        }
    }

    int ultima_carta = 0;
    for (int i = 0; i < num_cartas; i++) {
        if (i == num_cartas - 1) {
            if (menorMultiploDeQuatro(indices[i]) == menorMultiploDeQuatro(indices[i - 1])) {
                ultima_carta = 1;
            }
        }
    }
    // Retorna 1 se ambas as sequências foram encontradas
    if (sequencia_impares == (num_cartas/2)-1 && sequencia_pares == (num_cartas/2)-1 && ultima_carta==1) return 1;
    else return 0;

    return 0;
}


int main() {
    char *conjuntos = "🂴🂳🂢🂲🂤🂣";
    printf("%d\n", dupla_sequencia(conjuntos));
    return 0;
}