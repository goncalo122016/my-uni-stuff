#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

/*
 * Correção para o Privilege Escalation via passwd leak (Secção 2).
 *
 * Problema original:
 *   O FD de /etc/passwd aberto com permissões de escrita não era fechado
 *   antes de largar privilégios, sendo herdado pela shell. Um atacante
 *   podia escrever para /etc/passwd via /proc/self/fd/<fd> e criar um
 *   utilizador com UID 0 (root), obtendo acesso root total.
 *
 * Correção aplicada:
 *   Usar O_CLOEXEC ao abrir o ficheiro. Esta flag garante que o FD é
 *   fechado automaticamente quando execl faz o exec da shell, impedindo
 *   que a shell herde o acesso de escrita a /etc/passwd.
 *   Alternativa equivalente: chamar close(fd) antes de setuid/execl.
 */

int main() {
    /* CORREÇÃO: usar O_CLOEXEC para fechar automaticamente no exec */
    int fd = open("/etc/passwd", O_WRONLY | O_APPEND | O_CLOEXEC);
    if (fd < 0) {
        perror("open /etc/passwd");
        exit(1);
    }

    printf("Passwd FD: %d (sera fechado no exec)\n", fd);
    setuid(getuid());
    execl("/bin/sh", "sh", NULL);
    perror("execl");
    return 0;
}
