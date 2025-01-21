#include <stdio.h>
#include <ctype.h>
#include "histograma.h"

void ajusta(char s[]) {
    // Converter para minusculas e remover pontuação
    for (int i = 0; s[i]; i++) {
        if (isupper(s[i])) {
            s[i] = tolower(s[i]);
        } else if (ispunct(s[i])) {
            s[i] = '\0';
            return;
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <filename>\n", argv[0]);
        return 1;
    }
    FILE *f = fopen(argv[1], "r");
    if (f == NULL) {
        printf("Error opening file %s\n", argv[1]);
        return 2;
    }
    char palavra[50];
    Histogram h;
    init(h);

    while (fscanf(f, "%s", palavra) == 1) {
        ajusta(palavra);
        printf("%s\n", palavra);
    }

    liberta(h);
    fclose(f);
    return 0;
}