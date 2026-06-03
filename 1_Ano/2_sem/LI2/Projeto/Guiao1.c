#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

// Função para dar o índice onde a carta aparece no array
int carta_para_cod(const char *carta) {
    const char *baralho = "🂡🂱🃁🃑🂢🂲🃂🃒🂣🂳🃃🃓🂤🂴🃄🃔🂥🂵🃅🃕🂦🂶🃆🃖🂧🂷🃇🃗🂨🂸🃈🃘🂩🂹🃉🃙🂪🂺🃊🃚🂫🂻🃋🃛🂬🂼🃌🃜🂭🂽🃍🃝🂮🂾🃎🃞";
    int num_cartas = strlen(baralho) / 4;

    // Cada carta é representada por 4 caracteres
    for (int i = 0; i < num_cartas; i++) {
        // A função memcmp compara os primeiros 4 bytes da carta atual com os 4 primeiros bytes da carta que está sendo passada
        if (memcmp(carta, &baralho[i * 4], 4) == 0) {
            return i + 1;
        }
    }
    return -1;
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

// Verificar se uma lista de cartas possui o mesmo valor
int mesmo_valor(const char *cartas) {
    int comprimento = strlen(cartas);
    int indices[comprimento / 4]; // Cada carta é representada por 4 bytes
    indices_array(cartas, indices); // Preenche o array de índices
    for (int i = 0; i < comprimento / 4 - 1; i++) {
        if (menorMultiploDeQuatro(indices[i]) != menorMultiploDeQuatro(indices[i + 1])) {
            return 0; // Se pelo menos duas cartas tiverem multiplos de 4 diferentes, retorna 0
        }
    }
    return 1; // Se todas as cartas tiverem o mesmo multiplo de 4 correspondente, retorna 1
}

// Função compara para o qsort
int compara(const void *a, const void *b) {
    return (*(int *)a - *(int *)b);
}

// Verficar se uma lista de cartas é uma sequência
int sequencia(const char *cartas) {
    int num_cartas = strlen(cartas) / 4; // cada carta ocupa 4 bytes
    if (num_cartas <= 2) return 0;

    int indices[num_cartas]; // Cada carta é representada por 4 caracteres
    indices_array(cartas, indices); // Preenche o array de índices

    qsort(indices, num_cartas, sizeof(int), compara); // organizar os indices

    for (int i = 0; i < num_cartas - 1; i++) {
        if (menorMultiploDeQuatro(indices[i]) + 4 != menorMultiploDeQuatro(indices[i + 1])) {
            return 0; // uma lista de cartas é sequencia quando
        }             // o multiplo de quatro relativo a cada número associado a uma carta
                      // é resultado da subtração por 4 do número que se encontra na posição seguinte
    }

    return 1;
}

// Verificar se se trata de uma dupla_ sequencia
int dupla_sequencia(const char *cartas) {
    int num_cartas = strlen(cartas) / 4;
    if (num_cartas <= 4) return 0;

    if ((num_cartas % 2) == 0){
     
    int indices[num_cartas]; // Cada carta é representada por 4 caracteres
    indices_array(cartas, indices); // Preenche o array de índices

    qsort(indices, num_cartas, sizeof(int), compara);

    for (int i = 0; i < num_cartas - 1; i++) {
        if ((menorMultiploDeQuatro(indices[i]) == menorMultiploDeQuatro(indices[i + 1])) &&
            (menorMultiploDeQuatro(indices[i + 2]) == 4 + menorMultiploDeQuatro(indices[i + 1]))) {
            return 1; // uma lista de cartas é sequencia quando
        }             // o multiplo de quatro relativo a cada número associado a uma carta
                      // é resultado da subtração por 4 do número que se encontra na posição seguinte
    }
    }
    else{
        return 0;
    }
    return 0;
}

// Determinar a carta mais alta dentro de uma mão de cartas
char *carta_mais_alta(const char *cartas) {
    int comprimento = strlen(cartas);
    const char *maior_carta = &cartas[0]; // Inicializa a maior carta como a primeira da lista

    // Encontrar a carta mais alta
    for (int i = 4; i < comprimento; i += 4) { // Começa da segunda carta na lista
        if (carta_para_cod(&cartas[i]) > carta_para_cod(maior_carta)) {
            maior_carta = &cartas[i];
        }
    }

    static char resultado[5]; // 4 caracteres para a carta + 1 para o caractere nulo
    strncpy(resultado, maior_carta, 4);// Copiar a carta mais alta para a string de retorno
    resultado[4] = '\0';

    return resultado;
}

/*
int main() {
    int num_expressoes; // recebe o número de expressões

    if (scanf("%d", &num_expressoes) != 1) {
        printf("Erro ao ler o número de expressões.\n");
        return 1;
    }

    while (getchar() != '\n'); // O loop continuará até que o utilizador clique na tecla "enter", posteriormente 
                               //o buffer de entrada é limpo evitando, possíveis erros de leitura.


    char final[num_expressoes + 1][100]; // array que irá armazenar os "printf" relativos a cada linha de input (este array vai ser exibido no final)

    for (int i = 1; i <= num_expressoes; i++) {
        char cartas[1000];
        assert(fgets(cartas, sizeof(cartas), stdin) != NULL); // Lê a linha de cartas
        cartas[strlen(cartas) - 1] = '\0'; // Remove o caractere de nova linha do final

        int comprimento = strlen(cartas) / 4; // Comprimento do conjunto de cartas

        if (comprimento == 0) { // Se nenhuma carta for recebida
            strcpy(final[i], "Nada!");
        } else if (comprimento == 1) { // Verifica se apenas uma carta foi recebida
            sprintf(final[i], "conjunto com 1 cartas onde a carta mais alta é %s", cartas); // a função sprintf é uma função já definida no "<string.h>" que permite armazenar um dterminado "printf" num array.
        } else if (dupla_sequencia(cartas)) {
            sprintf(final[i], "dupla sequência com %d cartas onde a carta mais alta é %s", comprimento / 2, carta_mais_alta(cartas));
        } else if (mesmo_valor(cartas)) {
            sprintf(final[i], "conjunto com %d cartas onde a carta mais alta é %s", comprimento, carta_mais_alta(cartas));
        } else if (sequencia(cartas)) {
            sprintf(final[i], "sequência com %d cartas onde a carta mais alta é %s", comprimento, carta_mais_alta(cartas));
        } else {
            strcpy(final[i], "Nada!");
        }
    }

    for (int i = 1; i <= num_expressoes; i++) {
        printf("%s\n", final[i]);
    }

    return 0;
}
*/