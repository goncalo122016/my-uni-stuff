#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    FILE *f;
    char c;

    if (argc != 2) {
        printf("Uso: %s <ficheiro>\n", argv[0]);
        return 1;
    }

    f = fopen(argv[1], "r");

    if (f == NULL) {
        perror("Erro ao abrir ficheiro");
        return 1;
    }

    while ((c = fgetc(f)) != EOF) {
        putchar(c);
    }

    fclose(f);
    return 0;
}
