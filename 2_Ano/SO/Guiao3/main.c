#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <sys/wait.h> /* chamadas wait*() e macros relacionadas */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void ex1(){
    execl("/bin/ls", "ls", "-l", NULL);
}

void ex2(){
    int status;
    pid_t pid = fork();

    if (pid == -1) {
        perror("fork");
    } else if(pid == 0){
        printf("[FILHO]:\n");
        execl("/bin/ls", "ls", "-l", NULL);
    }
    else{
        pid_t terminated_pid = wait(&status);
        if (WIFEXITED(status)) {
            printf("[PAI]: O filho %d terminou com o código %d\n", terminated_pid, WEXITSTATUS(status));
        }
        else {
            printf("[PAI]: ERRO\n");
        }
    }
}

// Exercio 3
void my_system(char command[]){
    char* cmd;
    char* tokens[10];
    int i = 0;

    cmd = strtok(command, " ");
    while (cmd != NULL && i < 10) {
        tokens[i++] = cmd;
        cmd = strtok(NULL, " ");
    }

    tokens[i] = NULL;

    execvp(tokens[0], tokens);
}

int main(int argc, char* argv[]){
    //ex1();
    //ex2();
    
    // execlp("ls", "ls", "-l", NULL);

    // char* a[] = {"ls", "-l", NULL};
    // execvp("/usr/bin/ls", a);

    // execvp("ls", argv);

    char cmd[] = "ls -l";
    my_system(cmd);

    return 0;
}