#include <stdio.h>

void desenhaQuadrado (int l) {
    for (int i = 0; i<l; i++ ) {
        for (int j = 0; j<l; j++) {
            putchar ('#');
        }
        printf ("\n");
    }
}

void desenhaXadrez (int l1) {
    for (int i=0; i<l1; i++) {
        for (int j=0; j<l1; j++) {
            if ((i+j)%2 == 0) 
                putchar ('#');
            else 
                putchar ('_');
        }
    printf ("\n");
    }
}

void desenhaCirculo (int r, int *ncardinal) {

    for (int y = -r; y <= r; y++) {
        for (int x = -r; x <= r; x++) {
            // Verifica se o ponto (x, y) está dentro do círculo
            if (x * x + y * y <= r * r) {
                printf("#"); // Pertence ao círculo, imprime #
                (*ncardinal)++;
            } else {
                printf(" "); // Não pertence ao círculo, imprime espaço em branco
            }
        }
        printf("\n");
    } 
}

void triang1(int n) {
    for(int i = 1; i < 2 * n; i++) {
        for(int j = 1; j <= 2 * n - i; j++) {
            if(j <= i) putchar('#');
        }
        putchar('\n');
    }
}

void triang2(int n) {
    for(int i = 1; i <= n; i++) {
        int j = i - 1;
        for(int k = 0; k < n - 1 - j; k++) putchar(' ');
        for(int k = 0; k < 1 + 2 * j; k++) putchar('#');
        putchar('\n');
    }
}

int main () {
    desenhaQuadrado (5);
    desenhaXadrez (5);
    triang1(5);
    triang2(5);

    int raio = 4;
    int ncardinal = 0; 

    printf ("Introduza o raio do círculo que pretende:\n");
    scanf ("%d", &raio);
    desenhaCirculo (raio, &ncardinal);
    printf ("O número de # impressos é de: %d\n", ncardinal);

    }

// 
