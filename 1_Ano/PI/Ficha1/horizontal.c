#include <stdio.h>

void triangulo (int a) {

    // Triângulo crescente
    for (int i = 1; i <= a; i++) {
        for (int j = 1; j <= i; j++) {
            printf("#");
        }
        printf("\n");
    }

    // Triângulo decrescente
    for (int i = a - 1; i >= 1; i--) {
        for (int j = 1; j <= i; j++) {
            printf("#");
        }
        printf("\n");
    }
}