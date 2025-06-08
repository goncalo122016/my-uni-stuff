#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>

#define MAX 100000

void ex1(int message){
    int pd[2], status;
    pipe(pd);

    pid_t pid = fork();
    if(pid == -1){
        perror("fork");
        return;
    }

    if(pid == 0){
        int buffer;
        read(pd[0], &buffer, sizeof(int));
        printf("[Filho - Recebido]: %d\n", buffer);
        _exit(0);
    }
    else{
        sleep(7);
        write(pd[1], &message, sizeof(int));
        wait(&status);
    }
}

void ex2(){
    int pd[2], status;
    pipe(pd);

    pid_t pid = fork();
    if(pid == -1){
        perror("fork");
        return;
    }

    if(pid == 0){
        int message = 10;
        write(pd[1], &message, sizeof(int));
        wait(&status);
        _exit(0);
    }
    else{
        sleep(7);
        int buffer;
        read(pd[0], &buffer, sizeof(int));
        printf("[Pai - Recebido]: %d\n", buffer);
    }
}

void ex2_1 (){
    int pd[2], status;
    pipe(pd);

    pid_t pid = fork();
    if(pid == -1){
        perror("fork");
        return;
    }

    if(pid == 0){
        // FILHO
        close(pd[0]);
        for (int i = 0; i < MAX; i++){
            write(pd[1], &i, sizeof(int));
            printf("[Filho - Enviado]: %d\n", i);
        }
        close(pd[1]);
        _exit(0);
    }
    else{
        // PAI
        int i;

        close(pd[1]);
        close(pd[0]);
        while ((read(pd[0], &i, sizeof(int))) > 0){
            printf("[Pai - Recebido]: %d\n", i);
        }
        wait(&status);
    }
}

int main(){
    //int pd[2];
    //pipe(pd);
//
    //write(pd[1], "Hello", 5);
//
    //char buffer[5];
    //read(pd[0], buffer, 5);
//
    //printf("%s\n", buffer);

    //ex1(10);
    //ex2();
    ex2_1();
    return 0;
}