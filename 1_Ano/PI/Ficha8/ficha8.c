#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct slist {
    int valor;
    struct slist *prox;
} * LInt;

LInt newLInt (int x, LInt xs) {
    LInt r = malloc (sizeof(struct slist));
    if (r!=NULL) {
    r->valor = x; r->prox = xs;
    }
    return r;
}

typedef LInt Stack;

typedef struct {
    LInt inicio,fim;
} Queue;

void initStack (Stack *s) {
    (*s) = NULL;
}

int SisEmpty (Stack s) {
    return (s == NULL);
}

int push(Stack *s, int x) {
    LInt newLInt = malloc(sizeof(struct slist));
    if (newLInt == NULL) {
        return 1;
    }
    newLInt->valor = x;
    newLInt->prox = (*s);
    (*s) = newLInt;
    return 0;
}

int pop (Stack *s, int *x) {
    if (SisEmpty(*s)) {
        return 1;
    }
    *x = (*s)->valor;
    LInt aux = (*s);
    (*s) = (*s)->prox;
    free(aux);
    return 0;
}

int top (Stack s, int *x) {
    if (SisEmpty(s)) {
        return 1;
    }
    *x = s->valor;
    return 0;
}

void imprimeL (Stack l) {
    Stack aux = l;
    while (aux != NULL) {
        printf("%d\n", aux->valor);
        aux = aux->prox;
    }
}

//QUEUE

void initQueue (Queue *q) {
    q->inicio = q->fim = NULL;
}

int QisEmpty (Queue q) {
    return ((&q)->inicio == NULL);
}

int enqueue (Queue *q, int x) {
    LInt newLInt = malloc(sizeof(struct slist));
    newLInt->valor = x;
    newLInt->prox = NULL;
    if (QisEmpty(*q)) {
        q->inicio = q->fim = newLInt;
    } else {
        q->fim->prox = newLInt;
        q->fim = newLInt;
    }
    return 0;
}

int dequeue (Queue *q, int *x) {
    if (QisEmpty(*q)) {
        return 1;
    }
    (*x) = q->inicio->valor;
    LInt aux = q->inicio;
    q->inicio = q->inicio->prox;
    if (q->inicio == NULL) {
        q->fim = NULL;
    }
    free(aux);
    return 0;
}

int front(Queue q, int *x) {
    if (QisEmpty(q)) {
        return 1;
    }
    (*x) = (&q)->inicio->valor;
    return 0;
}

int main() {
    int i, a, b;
    Stack s;
    Queue q;
    //Deque d;
    printf("_______________ Testes _______________\n\n");
    printf("Stack:\n");
    initStack(&s);
    push(&s, 1);
    push(&s, 1);
    
    // altere este código de forma a que a stack tenha no final
    // do ciclo a squencia com os numeros de fibonacci
    for (i = 0; i < 10; i++) {
        pop(&s, &a);
        top(s, &b);
        push (&s, a);
        push(&s, (a + b));
    }
    while (!SisEmpty(s)) {
        pop(&s, &a);
        printf("%d ", a);
    }

    printf("\nQueue:\n");
    initQueue(&q);
    enqueue(&q, 1);
    enqueue(&q, 1);
    for (i = 0; i < 10; i++) {
        dequeue(&q, &a);
        front(q, &b);       
        enqueue (&q, a);
        enqueue(&q, (a + b));
    }
    while (!QisEmpty(q)) {
        dequeue(&q, &a);
        printf("%d ", a);
    }

    //printf("\nDeque:\n");
    //initDeque(&d);
    //pushFront(&d, 1);
    //pushBack(&d, 1);
    //for (i = 0; i < 10; i++) {
    //    popBack(&d, &a);
    //    back(d, &b);
    //    pushBack(&d, a);
    //    pushFront(&d, a + b);
    //}
//
    //popMax(&d, &a);
    //printf("Max: %d \n", a);
//
    //while (!DisEmpty(d)) {
    //    popBack(&d, &a);
    //    printf("%d ", a);
    //}

    printf("\n\n___________ Fim dos Testes ___________\n\n");
    return 0;
}
