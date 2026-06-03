#include <stdio.h>

void swapM (int *x, int *y) {
    int aux = *x;
    *x = *y;
    *y = aux;
}

void swap (int v[], int i, int j) {
    int aux = v[i];
    v[i] = v[j];
    v[j] = aux;
}

int soma (int v[], int N) {
    int s=0;
    for(int i=0; i<N; i++) {
        s+= v[i];
    }
    return s;
}

void inverteArray (int v[], int N) {
    int vAux[N];
    for(int i=0; i<N; i++) {
        vAux[i] = v[N-1-i];
    }
    for (int j=0; j<N; j++ ) {
        v[j]=vAux[j];
    }
}

void inverteArray2 (int v[], int N) {
    int i;
    for (i = 0; i < (N / 2); i++) {
        swapM(&v[i], &v[N - i - 1]);
    }
}

int maximum (int v[], int N, int *m) {
    *m = v[0];
    for(int i = 1; i<N; i++){
        if(v[i] > *m) *m = v[i];
    }    
    return 0;
}

void quadrados (int q[], int N) {
    for (int i=0; i<N; i++) {
        q[i] = i * i;
    }
}

void pascal (int v[], int N) {
    if (N==0) v[0] = 1;
    else {
        int prevLine[N-1];
        pascal(prevLine, N-1);
        v[0] = 1;
        v[N-1] = 1;
        for  (int i=1; i<N-1; i++) v[i] = prevLine[i-1] + prevLine [i];
    }
}

void triangulo_pascal (int N) {    
    for(int i = 1; i <= N; i++) {
        int j = i - 1;
        int v[i];
        pascal(v, i);

        for(int k = 0; k<N - 1 - j; k++) putchar(' ');
        for(int t = 0; t<i ; t++) printf ("%d ", v[t]);
        putchar('\n');
    }
}

int main() {
    int n, x=3, y=5;
    int v [10] =  {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    int t [10] =  { -1, -2, -3, -4, -5, -6, -7, -8, -9};
    int u [3];

    //swapM (&x, &y);
    //printf ("Swap x/y: %d %d\n", x, y);

    //swap (v,1,3);
    //for (int k = 0; k < 10; k++) printf ("%d  ", v[k]);
    //printf ("\n");
    //printf ("Soma: %d\n", soma (v,10));
    
    //inverteArray2 (v, 10);
    //printf ("Vetor Invertido: ");
    //for (int k = 0; k < 10; k++) printf ("%d ", v[k]);
    //putchar ('\n');

    
    //printf ("Valor máximo: %d\n", maximum (v, 10,  &v[0]));
    //printf ("Valor máximo: %d\n", maximum (t, 10,  &t[0]));

    //quadrados (u,10);
    //for (int k = 0; k < 10; k++) printf ("%d ", u[k]);
    //printf("\n");

    //pascal (u,3);
    //for (int k = 0; k < 3; k++) printf ("%d ", u[k]);
    //printf("\n");

    scanf ("%d", &n);
    triangulo_pascal(n);

    return 0;
}