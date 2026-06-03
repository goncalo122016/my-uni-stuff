#!/bin/bash

# Exercício 1
#include <stdio.h>
#include <stdlib.h>

#int main(int argc, char *argv[]) {
#    FILE *f;
#    char c;
#
#    if (argc != 2) {
#        printf("Uso: %s <ficheiro>\n", argv[0]);
#        return 1;
#    }
#
#    f = fopen(argv[1], "r");
#
#    if (f == NULL) {
#        perror("Erro ao abrir ficheiro");
#        return 1;
#    }
#
#    while ((c = fgetc(f)) != EOF) {
#        putchar(c);
#    }
#
#    fclose(f);
#    return 0;
#}
gcc sec3.c -o sec3

# Exercício 2
user add -m userssi

# Exercício 3
chown userssi sec3
chown userssi braga.txt

# Exercício 4
./sec3 braga.txt 
# Erro ao abrir ficheiro: Permission denied

# Exercício 5
chmod u+s readfile
# Com o setuid ativada no executável, o processo passa a executar com o UID efetivo do dono do ficheiro (userssi), mesmo quando é invocado por outro utilizador. Isto permite que o programa aceda ao ficheiro braga.txt, apesar do utilizador real não ter permissões diretas sobre o mesmo.

