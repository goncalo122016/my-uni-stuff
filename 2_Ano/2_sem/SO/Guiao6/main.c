#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <fcntl.h>

void ex3() {
    int fd_in = open("/etc/passwd", O_RDONLY);
    int fd_out = open("saida.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
    int fd_err = open("erros.txt", O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd_in == -1 || fd_out == -1 || fd_err == -1) {
        perror("Error opening file");
        return ;
    }

    int stdin_backup = dup(0);
    int stdout_backup = dup(1);
    int stderr_backup = dup(2);

    dup2(fd_in, 0);
    dup2(fd_out, 1);
    dup2(fd_err, 2);

    close(fd_in);
    close(fd_out);
    close(fd_err);

    execlp("wc", "wc", NULL);

    perror("Error executing wc");

    dup2(stdin_backup, 0);
    dup2(stdout_backup, 1);
    dup2(stderr_backup, 2);
    close(stdin_backup);
    close(stdout_backup);
    close(stderr_backup);
}

void ex4() {
    int fildes[2];
    pipe(fildes);

    pid_t pid = fork();
    if (pid == 0) {
        // FILHO
        close(fildes[1]);

        dup2(fildes[0], 0);
        close(fildes[0]);

        execlp("wc", "wc", NULL);
        _exit(0);
    } else {
        // PAI
        close(fildes[0]);

        char buffer[1024];
        ssize_t bytes_read;
        while ((bytes_read = read(0, buffer, 1024)) > 0) {
            write(fildes[1], buffer, bytes_read);
        }

        close(fildes[1]);

        wait(NULL);
    }
}

void ex5() {
    int fildes[2];
    pipe(fildes);

    pid_t pid = fork();
    if (pid == 0) {
        // FILHO
        close(fildes[1]);

        dup2(fildes[0], 0);
        close(fildes[0]);

        execlp("wc", "wc", "-l", NULL);
        _exit(0);
    } else {
        // PAI
        close(fildes[0]);

        dup2(fildes[1], 1);
        
        execlp("ls", "ls", "/etc", NULL);

        close(fildes[1]);

        wait(NULL);
    }
}

int main() {
    // ex3();
    //ex4();
    ex5();

    return 0;
}