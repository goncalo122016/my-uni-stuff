#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

// Tabela hash
typedef struct {
    int *tabela1;
    int *tabela2;
} HashCuckoo;

// Função de hash simples
int hash(int elemento, int tamanho) {
    return elemento % tamanho;
}

// Função de hash 2
int hash2(int elemento, int tamanho) {
    return (elemento / tamanho) % tamanho;
}

// Inicializa a tabela CUCKOO
void inicializarCuckoo(HashCuckoo *tabelaHash, int tamanho) {
    tabelaHash->tabela1 = malloc(tamanho * sizeof(int));
    tabelaHash->tabela2 = malloc(tamanho * sizeof(int));
    if (tabelaHash->tabela1 == NULL || tabelaHash->tabela2 == NULL) {
        printf("Erro na alocação de memória!\n");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < tamanho; i++) {
        tabelaHash->tabela1[i] = 0;
        tabelaHash->tabela2[i] = 0;
    }
}

// Insere uma chave na tabela hash usando a lógica de Cuckoo hashing
void inserirCuckoo(HashCuckoo *tabelaHash, int x, int tamanho, int tentativas) {
    int realocado;
    int indice1 = hash(x, tamanho), indice2 = hash2(x, tamanho);
    
    while (tentativas <= tamanho) {
        if (tentativas % 2 == 0){
            if (tabelaHash->tabela1[indice1] == 0){
                tabelaHash->tabela1[indice1] = x;
                printf("0 %d -> %d\n", indice1, x);
                printf ("OK\n");
                break;
            }
            else{
                tentativas++;
                realocado = tabelaHash->tabela1[indice1];
                tabelaHash->tabela1[indice1] = x;
                printf("0 %d -> %d\n", indice1, x);
        
                if (tentativas <= tamanho) inserirCuckoo(tabelaHash, realocado, tamanho, tentativas);
                break;
            }
        }
        else{
            if (tabelaHash->tabela2[indice2] == 0){
                tabelaHash->tabela2[indice2] = x;
                printf("1 %d -> %d\n", indice2, x);
                printf ("OK\n");
                break;
            }
            else{
                tentativas++;
                realocado = tabelaHash->tabela2[indice2];
                tabelaHash->tabela2[indice2] = x;
                printf("1 %d -> %d\n", indice2, x);
                if (tentativas <= tamanho) inserirCuckoo(tabelaHash, realocado, tamanho, tentativas);
                break;
            }
        }
    }
    if (tentativas > tamanho) printf("GIVING UP!\n");
}

// Procura uma chave na tabela CUCKOO
int procurarCuckoo(HashCuckoo *tabelaHash, int x, int tamanho) {
    int indice1 = hash(x, tamanho);
    int indice2 = hash2(x, tamanho);

    if (tabelaHash->tabela1[indice1] == x) {
        printf("%d\t%d\n", 0, indice1);
        return 1; // Chave encontrada na tabela1
    } else if (tabelaHash->tabela2[indice2] == x) {
        printf("%d\t%d\n", 1, indice2);
        return 1; // Chave encontrada na tabela2
    } else {
        printf("NO\n");
        return 0; // Chave não encontrada
    }
}

// Remove uma chave da tabela CUCKOO
int removerCuckoo(HashCuckoo *tabelaHash, int x, int tamanho) {
    int indice1 = hash(x, tamanho);
    int indice2 = hash2(x, tamanho);

    if (tabelaHash->tabela1[indice1] == x) {
        tabelaHash->tabela1[indice1] = 0;
        printf("OK\n");
        return 1; // Chave removida com sucesso
    } else if (tabelaHash->tabela2[indice2] == x) {
        tabelaHash->tabela2[indice2] = 0;
        printf("OK\n");
        return 1; // Chave removida com sucesso
    } else {
        printf("NO\n");
        return 0; // Chave não encontrada para remoção
    }
}

// Imprime a tabela CUCKOO
void imprimirCuckoo(HashCuckoo *tabelaHash, int tamanho) {
    for (int i = 0; i < tamanho; i++) {
        if (tabelaHash->tabela1[i] != 0) {
            printf("%d\t%d\t%d\n", 0, i, tabelaHash->tabela1[i]);
        }
    }
    for (int i = 0; i < tamanho; i++) {
        if (tabelaHash->tabela2[i] != 0) {
            printf("%d\t%d\t%d\n", 1, i, tabelaHash->tabela2[i]);
        }
    }
}

// Libera a memória alocada para a tabela CUCKOO
void libertaCuckoo(HashCuckoo *tabelaHash) {
    free(tabelaHash->tabela1);
    free(tabelaHash->tabela2);
}

// Função principal
int main() {
    int slots;
    HashCuckoo tabela;

    if (scanf("%d", &slots) != 1) {
        printf("Erro ao ler o número de slots\n");
        return 0;
    }

    char tipo_resolucao[7];
    if (scanf("%s", tipo_resolucao) != 1) {
        printf("Erro ao ler o tipo de resolução\n");
        return 0;
    }

    if (strcmp(tipo_resolucao, "CUCKOO") == 0){
        inicializarCuckoo(&tabela, slots);

        char comando;
        int chave;

        while (1) {
            // Lê o próximo caractere(comando)
            if (scanf(" %c", &comando) != 1) {
                printf("Erro ao ler o comando\n");
                break;
            }
            switch (comando) {  // Verifica o comando
                case 'I':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'I'\n");
                        break;
                    }
                    inserirCuckoo(&tabela, chave, slots, 0);
                    break;
                case 'D':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'D'\n");
                        break;
                    }
                    removerCuckoo(&tabela, chave, slots);
                    break;
                case 'C':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'C'\n");
                        break;
                    }
                    procurarCuckoo(&tabela, chave, slots);
                    break;
                case 'P':
                    imprimirCuckoo(&tabela, slots);
                    break;
                default:
                    printf("Comando inválido\n");
            }
        }

        libertaCuckoo(&tabela);
    }
    return 0;
}
