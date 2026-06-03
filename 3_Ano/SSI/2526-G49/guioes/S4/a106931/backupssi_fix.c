#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/*
 * Correção para o Capability Leaking da Secção 1.
 *
 * Problema original:
 *   O FD aberto para /root não era fechado antes de largar privilégios,
 *   sendo herdado pela shell com acesso total à diretoria protegida.
 *
 * Correção aplicada:
 *   1. Fechar explicitamente o dfd ANTES de chamar setuid(getuid()).
 *      Assim o FD deixa de existir quando os privilégios são largados
 *      e a shell não herda qualquer acesso privilegiado a /root.
 *   2. Alternativa: usar fcntl(dfd, F_SETFD, FD_CLOEXEC) para que o FD
 *      seja fechado automaticamente no execve (close-on-exec flag).
 */

int main() {
    int dfd;
    char *argv[2];

    dfd = open("/root", O_RDONLY);
    if (dfd == -1) {
        perror("open /root");
        exit(1);
    }
    printf("Directory FD is %d\n", dfd);

    if (mkdir("/root/backupssi", 0700) == -1) {
        perror("mkdir /root/backupssi");
    }

    /* CORREÇÃO: fechar o FD antes de largar privilégios */
    close(dfd);

    if (setuid(getuid()) == -1) {
        perror("setuid");
        exit(1);
    }

    argv[0] = "/bin/sh";
    argv[1] = NULL;
    execve(argv[0], argv, NULL);
    perror("execve");
    return 0;
}
