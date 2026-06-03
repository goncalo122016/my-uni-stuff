#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// Estrutura do nó da Deque
typedef struct Node {
    void* data;
    struct Node* next;
    struct Node* prev;
} Node;

// Estrutura da Deque
typedef struct Deque {
    Node* front;
    Node* back;
    int count;
} Deque;

// Função para criar e inicializar uma Deque
Deque* create(){
    Deque* deque = (Deque*)malloc(sizeof(Deque));
    deque->front = NULL;
    deque->back = NULL;
    deque->count = 0;
    return deque;
};

// Função para adicionar um elemento no final da Deque
void push(Deque* deque, void* data){
    Node* newnode = (Node*)malloc(sizeof(Node));
    newnode->data = data;
    newnode->next = NULL;
    if (deque->count == 0){
        newnode->prev = NULL;
        deque->front = newnode;
        deque->back = newnode;
        
    }
    else{
        newnode->prev = deque->back;
        deque->back->next = newnode;
        deque->back = newnode;
    }
    deque->count++;
};

// Função para adicionar um elemento no início da Deque
void pushFront(Deque* deque, void* data){
    Node* newnode = (Node*)malloc(sizeof(Node));
    newnode->data = data;
    newnode->prev = NULL;
    if (deque->count == 0){
        deque->front = newnode;
        deque->back = newnode;
        newnode->next = NULL;
    }
    else{
        newnode->next = deque->front;
        deque->front->prev = newnode;
        deque->front = newnode;    
    }
    deque->count++;
};

// Função para remover o elemento do final da Deque
void* pop(Deque* deque){
    if (deque->count == 0){
        return NULL;
    }
    Node* current = deque->back;
    deque->back = current->prev;
    deque->back->next = NULL;
    free(current);
    deque->count--;
};

// Função para remover o elemento do início da Deque
void* popFront(Deque* deque){
    if (deque->count == 0){
        return NULL;
    }
    Node* current = deque->front;
    deque->front = current->next;
    deque->front->prev = NULL;
    free(current);
    deque->count--;
};

// Função para retornar o número de elementos na Deque
int size(Deque* deque){
    return deque->count;
};

// Função para verificar se a Deque está vazia
bool isEmpty(Deque* deque){
    return deque->count == 0;
};

// Função para inverter a ordem dos elementos na Deque
void reverse(Deque* deque){
    Node* current = deque->front;
    Node* temp;
    while (current != NULL){
        temp = current->next;
        current->next = current->prev;
        current->prev = temp;
        current = temp;
    }
    temp = deque->front;
    deque->front = deque->back;
    deque->back = temp;
};

// Função para imprimir os elementos da Deque
void printDeque(Deque* deque, void(*printFunc)(void*)){
    Node* current = deque->front;
    while (current != NULL){
        printFunc(current->data);
        current = current->next;
    }
    printf("\n");
};

// Função para destruir a Deque e liberar a memória
void destroy(Deque* deque){
    Node* current = deque->front;
    Node* temp;
    while (current != NULL){
        temp = current->next;
        free(current);
        current = temp;
    }
    free(deque);
};

// Exemplo de função de impressão para inteiros
void printInt(void* data){
    printf("%d ", *(int*)data);
};

int main() {
    Deque* deque = create();

    int values[] = {10, 20, 30, 40, 50};
    
    push(deque, &values[0]);
    push(deque, &values[1]);
    pushFront(deque, &values[2]);
    push(deque, &values[3]);
    pushFront(deque, &values[4]);

    printf("Deque após inserções: ");
    printDeque(deque, printInt);

    reverse(deque);
    printf("Deque após inversão: ");
    printDeque(deque, printInt);

    printf("Elemento removido do início: ");
    popFront(deque);
    printDeque(deque, printInt);
    printf("Elemento removido do fim: ");
    pop(deque);
    printDeque(deque, printInt);

    destroy(deque);
    return 0;
}