#include <stdio.h>

void triangulo (int a) {
    for (int i=1; i<=a; i++) {
    // Adiciona espaços à esquerda
        for (int j = 1; j <= a - i; j++) {
            printf(" ");
        }
    // Adiciona caracteres '#' após os espaços
        for (int k = 1; k <= 2 * i - 1; k++) {
            printf("#");
        }
    putchar ('\n');
    }
}