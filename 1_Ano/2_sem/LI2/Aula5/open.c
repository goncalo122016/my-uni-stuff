#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Estrutura para representar a tabela hash
typedef struct {
    int *slots;
    int tamanho;
    int *deleted;
} HashTable;

// Função de hash simples
int hash(int chave, int tamanho) {
    return chave % tamanho;
}

// Inicializa a tabela hash
void inicializarOpen(HashTable *tabela, int tamanho) {
    tabela->slots = (int *)malloc(tamanho * sizeof(int));
    tabela->deleted = (int *)malloc(tamanho * sizeof(int));
    tabela->tamanho = tamanho;
    memset(tabela->slots, -1, tamanho * sizeof(int)); // -1 indica que a célula está vazia
    memset(tabela->deleted, 0, tamanho * sizeof(int)); // 0 indica que a célula não está marcada para deleção
}

// Função para liberar a memória alocada para a tabela hash
void libertarOpen(HashTable *tabela) {
    free(tabela->slots);
    free(tabela->deleted);
}

// Função para inserir um número na tabela hash
void inserirOpen(HashTable *tabela, int numero) {
    int indice = hash(numero, tabela->tamanho);
    int indice_realocacao = -1; // Índice de realocação do número existente, inicializado como -1

    // Procura por uma posição vazia ou uma posição marcada para deleção
    while (tabela->slots[indice] != -1 && tabela->slots[indice] != numero && !tabela->deleted[indice]) {
        indice = (indice + 1) % tabela->tamanho;
        // Verifica se percorreu toda a tabela sem encontrar uma posição vazia ou marcada para deleção
        if (indice == hash(numero, tabela->tamanho)) {
            printf("GIVING UP!!!\n");
            return;
        }
    }

    // Verifica se o número já está presente na tabela
    for (int i = 0; i < tabela->tamanho; i++) {
        if (tabela->slots[i] == numero && !tabela->deleted[i]) {
            // Remove a ocorrência anterior do número
            tabela->deleted[i] = 0;
            tabela->slots[i] = -1;
            // Atualiza o índice de realocação
            indice_realocacao = i;
            break;
        }
    }

    // Se encontrou uma posição vazia ou marcada para deleção, insere o número nessa posição
    tabela->slots[indice] = numero;
    tabela->deleted[indice] = 0;

    if (indice_realocacao != -1) {
        printf("%d EXISTS\n", numero);
    } else {
        printf("%d -> %d\n", indice, numero); // Imprime a inserção
        printf("OK\n");
    }
}

// Função para buscar um número na tabela hash
void procurarOpen(HashTable *tabela, int numero) {
    int indice = hash(numero, tabela->tamanho);
    int indice_realocacao = -1; // Índice de realocação do número existente, inicializado como -1

    // Procura pelo número na tabela
    while (tabela->slots[indice] != -1) {
        if (tabela->slots[indice] == numero && !tabela->deleted[indice]) {
            // Realiza a tentativa de realocação do número existente
            for (int i = 0; i < tabela->tamanho; i++) {
                if (tabela->slots[i] == -1 || tabela->deleted[i]) {
                    // Remove a ocorrência anterior do número
                    tabela->deleted[indice] = 1;
                    // Insere o número na nova posição
                    tabela->slots[i] = numero;
                    tabela->deleted[i] = 0;
                    indice_realocacao = i; // Atualiza o índice de realocação
                    break;
                }
            }
            if (indice_realocacao != -1) {
                printf("%d\n", indice_realocacao); // Imprime a realocação
                return;
            }
            printf("%d\n", indice); // Retorna o índice onde o número foi encontrado
            return;
        }
        indice = (indice + 1) % tabela->tamanho;
        // Verifica se percorreu toda a tabela sem encontrar o número
        if (indice == hash(numero, tabela->tamanho)) {
            printf("NO\n");
            return;
        }
    }

    // Se chegou a uma posição vazia sem encontrar o número
    printf("NO\n");
}

// Função para remover um número da tabela hash
void removerOpen(HashTable *tabela, int numero) {
    int indice = hash(numero, tabela->tamanho);

    // Procura pela chave na tabela hash
    while (tabela->slots[indice] != -1) {
        if (tabela->slots[indice] == numero && !tabela->deleted[indice]) {
            tabela->deleted[indice] = 1; // Marca a célula como deletada
            printf("OK\n");
            return;
        }
        indice = (indice + 1) % tabela->tamanho;
        // Verifica se percorreu toda a tabela sem encontrar a chave
        if (indice == hash(numero, tabela->tamanho)) {
            printf("NO\n");
            return;
        }
    }

    // Se a chave não foi encontrada, imprime "NO"
    printf("NO\n");
}

// Função para imprimir a tabela hash
void imprimirOpen(HashTable *tabela) {
    for (int i = 0; i < tabela->tamanho; i++) {
        if (tabela->slots[i] != -1) {
            if (tabela->deleted[i]) {
                printf("%d\tD\n", i); // Imprime a posição marcada como "D" se estiver deletada
            } else {
                printf("%d\t%d\n", i, tabela->slots[i]); // Imprime o índice e o número armazenado
            }
        }
    }
}

int main() {
    int slots;
    char tipo_resolucao[10];
    HashTable tabela;

    if (scanf("%d", &slots) != 1) {
        printf("Erro ao ler o número de slots\n");
        return 0;
    }

    if (scanf("%s", tipo_resolucao) != 1) {
        printf("Erro ao ler o tipo de resolução\n");
        return 0;
    }

    if (strcmp(tipo_resolucao, "OPEN") == 0) {
        inicializarOpen(&tabela, slots);

        char operacao;
        int numero;
        // Loop para processar os comandos
        while (1) {
            // Lê o próximo caractere (comando) sem consumir espaços em branco
            if (scanf(" %c", &operacao) != 1) {
                printf("Erro ao ler o comando\n");
                break;
            }
            switch (operacao) {  // Verifica o comando
                case 'I':
                    if (scanf("%d", &numero) != 1) {
                        printf("Erro ao ler o número para o comando 'I'\n");
                        break;
                    }
                    inserirOpen(&tabela, numero);
                    break;
                case 'D':
                    if (scanf("%d", &numero) != 1) {
                        printf("Erro ao ler o número para o comando 'D'\n");
                        break;
                    }
                    removerOpen(&tabela, numero);
                    break;
                case 'C':
                    if (scanf("%d", &numero) != 1) {
                        printf("Erro ao ler o número para o comando 'C'\n");
                        break;
                    }
                    procurarOpen(&tabela, numero);
                    break;
                case 'P':
                    imprimirOpen(&tabela);
                    break;
                default:
                    printf("Comando inválido\n");
            }
        }
    }
    libertarOpen(&tabela);
    return 0;
}
/*
2 -> 7
OK
3 -> 2
OK
4 -> 12
OK
2	7
3	2
4	12
OK
12 EXISTS
2	7
3	12
4	D
4 -> 2
OK
0 -> 25
OK
1 -> 11
OK
OK
0	25
1	11
2	D
3	12
4	2
2 -> 4
OK
0	25
1	11
2	4
3	12
4	2
OK
0	D
1	11
2	4
3	12
4	2
0
0	4
1	11
2	D
3	12
4	2
2 -> 13
OK
0	4
1	11
2	13
3	12
4	2
OK
OK
0	D
1	D
2	13
3	12
4	2
13 EXISTS
0	13
1	D
2	D
3	12
4	2
2
0	13
1	D
2	2
3	12
4	D
3
0	13
1	D
2	2
3	12
4	D
4
0	D
1	D
2	2
3	12
4	13
0 -> 75
OK
1 -> 120
OK
0	75
1	120
2	2
3	12
4	13
OK
2 -> 11
OK
0	75
1	120
2	11
3	12
4	13
OK
3 -> 34
OK
0	75
1	120
2	11
3	34
4	13
OK
OK
OK
0	D
1	D
2	D
3	34
4	13
4
0
1 -> 79
OK
2 -> 39
OK
0	34
1	79
2	39
3	D
4	13
3
0	34
1	79
2	39
3	13
4	D
4
0	34
1	D
2	39
3	13
4	79
1
0	34
1	39
2	D
3	13
4	79
3
1
0
NO
NO
0	34
1	39
2	D
3	13
4	79
2 -> 38
OK
0	34
1	39
2	38
3	13
4	79
OK
38 EXISTS
0	34
1	39
2	D
3	13
4	38
0
OK
OK
OK
NO
OK
0	D
1	D
2	D
3	D
4	D
2 -> 9327
OK
1 -> 3276
OK
3 -> 3263
OK
4 -> 32832
OK
0 -> 123664
OK
0	123664
1	3276
2	9327
3	3263
4	32832
OK
32832 EXISTS
123664 EXISTS
0	D
1	3276
2	32832
3	3263
4	123664
1
2
3
4
0	D
1	3276
2	32832
3	3263
4	123664
OK
OK
OK
OK
0	D
1	D
2	D
3	D
4	D
2 -> 22
OK
3 -> 32
OK
1 -> 71
OK
4 -> 96
OK
0	D
1	71
2	22
3	32
4	96
OK
2
32 EXISTS
OK
1
0	D
1	96
2	D
3	32
4	D
4 -> 103
OK
0 -> 89
OK
0	89
1	96
2	D
3	32
4	103
2
3
4
0 -> 327686
OK
0	327686
1	96
2	32
3	103
4	89
GIVING UP!
*/