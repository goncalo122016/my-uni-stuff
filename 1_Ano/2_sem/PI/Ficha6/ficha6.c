#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define Max 10

typedef struct staticStack {
    int sp;
    int values [Max];
} STACK, *SStack;

typedef struct dinStack {
    int size; // guarda o tamanho do array values
    int sp;
    int *values;
} *DStack;

// Static stacks 

void SinitStack (SStack s){
    s->sp = 0;
}

int SisEmpty (SStack s){
	if (s->sp == 0) return 1;
    return 0;
}

int Spush (SStack s, int x){
    if (s->sp == Max) return 1;
    s->values[s->sp] = x;
    s->sp++;
	return 0;
}

int Spop (SStack s, int *x) {
	if (s->sp == 0) return 1;
    s->sp--;
    *x = s->values[s->sp];
	return 0;
}

int Stop (SStack s, int *x) {
	if (s->sp == 0) return 1;
    s->sp--;
    *x = s->values[s->sp];
    s->sp++;
	return 0;
}

void ShowSStack (SStack s){
    int i;
    printf ("%d Items: ", s->sp);
    for (i=s->sp-1; i>=0; i--) 
        printf ("%d ", s->values[i]);
    putchar ('\n');
}

// Stacks with dynamic arrays

int dupStack (DStack s) {
	int r = 0, i;
	int *t = malloc (2*s->size*sizeof(int));

	if (t == NULL) r = 1;
	else {
		for (i=0; i<s->size; i++) 
			t[i] = s->values[i];
		free (s->values);
		s->values = t;
		s->size*=2;
	}
	return r;
}

void DinitStack (DStack s) {
	s->size = Max;
    s->sp = 0;
    s->values = malloc(s->size * sizeof(int));
}

int DisEmpty (DStack s) {
	if (s->size == 0) return 1;
}

int Dpush (DStack s, int x){
	if (s->sp == s->size) {
        if (dupStack != 0) return 1;
    }
    s->values[s->sp] = x;
    s->sp++;
	return 0;
}

int Dpop (DStack s, int *x){
	if (s->sp == 0) return 1;
    s->sp--;
    *x = s->values[s->sp];
	return 0;
}

int Dtop (DStack s, int *x){
	if (s->sp == 0) return 1;
    s->sp--;
    *x = s->values[s->sp];
    s->sp++;
	return 0;
}

void ShowDStack (DStack s){
    int i;
    printf ("%d Items: ", s->sp);
    for (i=s->sp-1; i>=0; i--) 
        printf ("%d ", s->values[i]);
    putchar ('\n');
}

typedef struct staticQueue {
    int front;
    int length;
    int values [Max];
} QUEUE, *SQueue;

typedef struct dinQueue {
    int size; // guarda o tamanho do array values
    int front;
    int length;
    int *values;
} *DQueue;

// Static queues

void SinitQueue(SQueue q) {
    q->length = 0;
    q->front = 0;
}

int SisEmptyQ(SQueue q) {
    return q->length == 0;
}

int Senqueue(SQueue q, int x) {
    if (q->length == Max) {
        return 1;
    } 

    q->values[(q->front + q->length) % Max] = x;
    q->length++;

    return 0;
}

int Sdequeue(SQueue q, int *x) {
    if(SisEmptyQ(q)) {
        return 1;
    }

    (*x) = q->values[q->front];
    q->front = (q->front + 1) % Max;
    q->length--;

    return 0;
}

int Sfront(SQueue q, int *x) {
    if(SisEmptyQ(q)) {
        return 1;
    }

    (*x) = q->values[q->front];

    return 0;
}

void ShowSQueue(SQueue q) {
    int i, p;
    printf("%d Items: ", q->length);
    for (i = 0, p = q->front; i < q->length; i++) {
        printf("%d ", q->values[p]);
        p = (p + 1) % Max;
    }
    putchar('\n');
}

// Queues with dynamic arrays

int dupQueue(DQueue q) {
    q->size *= 2;
    int *valores = realloc(q->values, q->size * sizeof(int));

    if (valores == NULL) return 1;

    int i, k;

    for (k = q->size -1, i = q->length - 1; i >= q->front; i--, k--) {
        valores[k] = q->values[i];
    }

    q->front = k + 1;

    return 0;
}

void DinitQueue(DQueue q) {
    q->front = 0;
    q->length = 0;
    q->size = Max;
    q->values = malloc(q->size * sizeof(int));
}

int DisEmptyQ(DQueue q) {
    return q->length == 0;
}

int Denqueue(DQueue q, int x) {
    if (q->length == q->size) {
        if (dupQueue(q) == 1) {
            return 1;
        }
    } 

    q->values[(q->front + q->length) % q->size] = x;
    q->length++;

    return 0;
}

int Ddequeue(DQueue q, int *x) {
    if(DisEmptyQ(q)) {
        return 1;
    }

    (*x) = q->values[q->front];
    q->front = (q->front + 1) % q->size;
    q->length--;

    return 0;
}

int Dfront(DQueue q, int *x) {
    if(DisEmptyQ(q)) {
        return 1;
    }

    (*x) = q->values[q->front];

    return 0;
}

void ShowDQueue(DQueue q) {
    int i, p;
    printf("%d Items: ", q->length);
    for (i = 0, p = q->front; i < q->length; i++) {
        printf("%d ", q->values[p]);
        p = (p + 1) % q->size;
    }
    putchar('\n');
}

int main() {
    int i;
    // struct staticStack s1;
    // SStack S1 = &s1;
    // struct dinStack d1;
    // DStack D1 = &d1;

    struct staticQueue q1;
    SQueue Q1 = &q1;
    struct dinQueue r1;
    DQueue R1 = &r1;

    // printf("Testing Stacks .... \n");
    // SinitStack(S1);
    // DinitStack(D1);
    // for (i = 0; i < 15; i++) {
    //     if (Spush(S1, i) != 0) printf("ERROR pushing %d\n", i);
    //     if (Dpush(D1, i) != 0) printf("ERROR pushing %d\n", i);
    // }
    // ShowSStack(S1);
    // ShowDStack(D1);

    printf("Testing Queues .... \n");
    // SinitQueue(Q1);
    DinitQueue(R1);
    for (i = 0; i < 15; i++) {
        // if (Senqueue(Q1, i) != 0) printf("ERROR enqueueing %d\n", i);
        if (Denqueue (R1, i) != 0) printf("ERROR enqueueing %d\n", i);
    }
    // ShowSQueue(Q1);
    ShowDQueue(R1);

    int x;
    for (i = 0; i < 5; i++) {
        // if (Sdequeue(Q1, &x) != 0) printf("ERROR Denqueueing %d\n", i);
        // else printf("Dequeued %d\n", x);
        if (Ddequeue(R1, &x) != 0) printf("ERROR Denqueueing %d\n", i);
        else printf("Dequeued %d\n", x);
    }
    // ShowSQueue(Q1);
    ShowDQueue(R1);

    return 0;
}
