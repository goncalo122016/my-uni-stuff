#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

// OPEN
typedef struct {
    int *slots;         // Array para armazenar os elementos da tabela OPEN
    int tamanho;        // Tamanho da tabela OPEN
    int *deleted;       // Array para marcar as células que foram apagadas
} HashTable;

// Função de hash simples
int hash(int elemento, int tamanho) {
    return elemento % tamanho;
}

// Inicializa a tabela OPEN
void inicializarOpen(HashTable *tabela, int size) {
    tabela->slots = (int *)malloc(size * sizeof(int));         // Aloca memória para os slots
    tabela->deleted = (int *)malloc(size * sizeof(int));       // Aloca memória para o array para saber se foram apagados(1) ou não(0)
    tabela->tamanho = size;                                    // Define o tamanho da tabela
    memset(tabela->slots, -1, size * sizeof(int));             // Inicializa os slots com -1 (vazios)
    memset(tabela->deleted, 0, size * sizeof(int));            // Inicializa o array de marcação com 0 (não deletados)
}

// LiberTa a memória alocada para a tabela OPEN
void libertarOpen(HashTable *tabela) {
    free(tabela->slots);
    free(tabela->deleted);
}

// Função para inserir um número na tabela OPEN
void inserirOpen(HashTable *tabela, int numero) {
    int indice = hash(numero, tabela->tamanho);
    int indice_realocacao = -1; // Índice de realocação do número existente, inicializado como -1

    // Procura por uma posição vazia ou uma posição marcada para apagar
    while (tabela->slots[indice] != -1 && tabela->slots[indice] != numero && !tabela->deleted[indice]) {
        indice = (indice + 1) % tabela->tamanho;
        if (indice == hash(numero, tabela->tamanho)) {
            printf("GIVING UP!!!\n");
            return;
        }
    }

    // Verifica se o número já está presente na tabela
    for (int i = 0; i < tabela->tamanho; i++) {
        if (tabela->slots[i] == numero && !tabela->deleted[i]) {
            // Remove a ocorrência anterior do número
            tabela->deleted[i] = 1;
            indice_realocacao = i;
            break;
        }
    }

    tabela->slots[indice] = numero;
    tabela->deleted[indice] = 0;

    if (indice_realocacao != -1) {
        printf("%d EXISTS\n", numero); // Imprime se o número já existia na tabela
    } else {
        printf("%d -> %d\n", indice, numero); // Imprime a inserção caso contrário
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
    printf("NO\n"); // Se chegou a uma posição vazia sem encontrar o número
}

// Função para remover um número da tabela OPEN
void removerOpen(HashTable *tabela, int numero) {
    int indice = hash(numero, tabela->tamanho);

    while (tabela->slots[indice] != -1) {
        if (tabela->slots[indice] == numero && !tabela->deleted[indice]) {
            tabela->deleted[indice] = 1; // Marca a célula como apagada
            printf("OK\n");
            return;
        }
        indice = (indice + 1) % tabela->tamanho;
        if (indice == hash(numero, tabela->tamanho)) { // Verifica se percorreu a tabela toda sem encontrar a chave
            printf("NO\n");
            return;
        }
    }
    printf("NO\n");
}

// Função para imprimir a tabela OPEN
void imprimirOpen(HashTable *tabela) {
    for (int i = 0; i < tabela->tamanho; i++) {
        if (tabela->slots[i] != -1) {
            if (tabela->deleted[i]) {
                printf("%d\tD\n", i); // Imprime a posição marcada como "D" se tiver sido apagada
            } else {
                printf("%d\t%d\n", i, tabela->slots[i]);
            }
        }
    }
}

//LINK
typedef struct No {
    int chave;
    struct No* proximo;
} No;

// Estrutura para a tabela hash com resolução de conflitos por lista ligada
typedef struct {
    No** tabela;
} TabelaHashLigada;

// Inicializa a tabela LINK
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

// Insere um elemento na tabela LINK
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

// Verifica se um elemento está presente na tabela LINK
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

// Remove um elemento da tabela LINK
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

// Imprime a tabela LINK
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
// Libera a memória alocada para a tabela LINK
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

//CUCKOO
typedef struct {
    int *tabela1; //Tabela t1
    int *tabela2; //Tabela t2
} HashCuckoo;

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

int main() {
    int slots;

    // Lê o número de slots (P)
    if (scanf("%d", &slots) != 1) {
        printf("Erro ao ler o número de slots\n");
        return 0;
    }

    // Lê o tipo de resolução (OPEN, LINK ou CUCKOO)
    char tipo_resolucao[7];
    if (scanf("%s", tipo_resolucao) != 1) {
        printf("Erro ao ler o tipo de resolução\n");
        return 0;
    }

    // Verifica o tipo de resolução
    if (strcmp(tipo_resolucao, "OPEN") == 0){
        HashTable tabela;
        inicializarOpen(&tabela, slots);

        char operacao;
        int numero;
        while (scanf(" %c", &operacao) == 1) { // Loop para processar os comandos 
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
                    break;
            }
        }
        libertarOpen(&tabela);
    }
    else if (strcmp(tipo_resolucao, "LINK") == 0){
        TabelaHashLigada tabela;
        inicializarLink(&tabela, slots);

        char operacao;
        int chave;
        while (scanf(" %c", &operacao) == 1) { // Loop para processar os comandos
            switch (operacao) {  // Verifica o comando
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
                    break;
            }
        }
        libertarLink(&tabela, slots);
    }
    else if (strcmp(tipo_resolucao, "CUCKOO") == 0){
        HashCuckoo tabelaHash;
        inicializarCuckoo(&tabelaHash, slots);

        char operacao;
        int chave;
        while (scanf(" %c", &operacao) == 1) { // Loop para processar os comandos
            switch (operacao) {  // Verifica o comando
                case 'I':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'I'\n");
                        break;
                    }
                    inserirCuckoo(&tabelaHash, chave, slots, 0);
                    break;
                case 'D':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'D'\n");
                        break;
                    }
                    removerCuckoo(&tabelaHash, chave, slots);
                    break;
                case 'C':
                    if (scanf("%d", &chave) != 1) {
                        printf("Erro ao ler o número para o comando 'C'\n");
                        break;
                    }
                    procurarCuckoo(&tabelaHash, chave, slots);
                    break;
                case 'P':
                    imprimirCuckoo(&tabelaHash, slots);
                    break;
                default:
                    printf("Comando inválido\n");
                    break;
            }
        }
        libertaCuckoo(&tabelaHash);
    }
    else {
        printf("Tipo de resolução inválido\n");
        return 0;
    }
    return 0;
}