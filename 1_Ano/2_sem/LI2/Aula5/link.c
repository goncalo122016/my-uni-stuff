#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

// Estrutura de nó para lista ligada
typedef struct No {
    int chave;
    struct No* proximo;
} No;

// Estrutura para a tabela hash com resolução de conflitos por lista ligada
typedef struct {
    No** tabela;
} TabelaHashLigada;

// Função de hash simples
int hash(int chave, int tamanho) {
    return chave % tamanho;
}

// Inicializa a tabela hash
void inicializarLink(TabelaHashLigada* tabela, int tamanho) {
    tabela->tabela = (No**)malloc(tamanho * sizeof(No*));
    if (tabela->tabela == NULL) {
        printf("Falha na alocação de memória!\n");
        exit(EXIT_FAILURE);
    }
    for (int i = 0; i < tamanho; i++) {
        tabela->tabela[i] = NULL;
    }
}

// Insere um elemento na tabela hash
void inserirLink(TabelaHashLigada* tabela, int chave, int tamanho) {
    int indice = hash(chave, tamanho);
    No* atual = tabela->tabela[indice];

    while (atual != NULL) {
        if (atual->chave == chave) {
            printf("%d JÁ EXISTE\n", chave);
            return;
        }
        atual = atual->proximo;
    }

    No* novoNo = (No*)malloc(sizeof(No));
    if (novoNo == NULL) {
        printf("Falha na alocação de memória!\n");
        exit(EXIT_FAILURE);
    }
    novoNo->chave = chave;
    novoNo->proximo = tabela->tabela[indice];
    tabela->tabela[indice] = novoNo;

    printf("%d -> %d\n", indice, chave);
    printf("OK\n");
}

// Verifica se um elemento está presente na tabela hash
void procurarLink(TabelaHashLigada* tabela, int chave, int tamanho) {
    int indice = hash(chave, tamanho);
    No* atual = tabela->tabela[indice];

    while (atual != NULL) {
        if (atual->chave == chave) {
            printf("%d\n", indice);
            return;
        }
        atual = atual->proximo;
    }

    printf("NO\n");
}

// Remove um elemento da tabela hash
void removerLink(TabelaHashLigada* tabela, int chave, int tamanho) {
    int indice = hash(chave, tamanho);
    No* atual = tabela->tabela[indice];
    No* anterior = NULL;

    while (atual != NULL) {
        if (atual->chave == chave) {
            if (anterior == NULL) {
                tabela->tabela[indice] = atual->proximo;
            } else {
                anterior->proximo = atual->proximo;
            }
            free(atual);
            printf("OK\n");
            return;
        }
        anterior = atual;
        atual = atual->proximo;
    }

    printf("NO\n");
}

// Imprime a tabela hash
void imprimirLink(TabelaHashLigada* tabela, int tamanho) {
    for (int i = 0; i < tamanho; i++) {
        No* atual = tabela->tabela[i];
        if (atual != NULL) { // Verifica se a chave possui elementos associados
            printf("%d", i);
            while (atual != NULL) {
                printf(" %d", atual->chave);
                atual = atual->proximo;
            }
            printf("\n");
        }
    }
}
// Libera a memória alocada para a tabela hash
void libertarLink(TabelaHashLigada* tabela, int tamanho) {
    for (int i = 0; i < tamanho; i++) {
        No* atual = tabela->tabela[i];

        while (atual != NULL) {
            No* temp = atual;
            atual = atual->proximo;
            free(temp);
        }
    }
    free(tabela->tabela);
}

int main() {
    int slots;
    TabelaHashLigada tabela;

    if (scanf("%d", &slots) != 1) {
        printf("Erro ao ler o número de slots\n");
        return EXIT_FAILURE;
    }

    char tipo_resolucao[7];
    if (scanf("%s", tipo_resolucao) != 1) {
        printf("Erro ao ler o tipo de resolução\n");
        return EXIT_FAILURE;
    }
    inicializarLink(&tabela, slots);

    if (strcmp(tipo_resolucao, "LINK") == 0) {
        char comando;
        int chave;
        
        while (1) {
            // Lê o próximo caractere (comando) sem consumir espaços em branco
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
                    inserirLink(&tabela, chave, slots);
                    break;
                case 'D':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'D'\n");
                        break;
                    }
                    removerLink(&tabela, chave, slots);
                    break;
                case 'C':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'C'\n");
                        break;
                    }
                    procurarLink(&tabela, chave, slots);
                    break;
                case 'P':
                    imprimirLink(&tabela, slots);
                    break;
                default:
                    printf("Comando inválido\n");
            }
    }

        libertarLink(&tabela, slots);
    }
    return 0;
}
