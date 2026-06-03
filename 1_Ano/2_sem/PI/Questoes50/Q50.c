#include <stdio.h>	
#include <stdlib.h>
#include <string.h>

typedef enum movimento {Norte, Oeste, Sul, Este} Movimento;
typedef struct posicao {
    int x, y;
} Posicao;

int remRep(char x[]) {
    if (x[0] == '\0') return 0;

    int cont = 0;
    for (int i = 0; x[i]; i++) {
        if (x[i] == x[i+1]) {
            for (int j = i; x[j]; j++) x[j] = x[j+1];
        }
        else cont++;
    }
    return cont;
}

int main() {
    char x[] = "aaabaaabbbaaa";
    int length = remRep(x);
    printf("String após remoção de caracteres repetidos: %s\n", x);
    printf("Comprimento da string resultante: %d\n", length);
    return 0;
}