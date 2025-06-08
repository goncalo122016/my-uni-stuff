#include <sys/types.h>
#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <fcntl.h> /* O_RDONLY, O_WRONLY, O_CREAT, O_* */
#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 1024

// Exercicio 1
void mycat(){
    char buffer[MAX_SIZE];
    int bytesRead;
    while((bytesRead = read(0, buffer, MAX_SIZE)) > 0){
        write(1, buffer, bytesRead);
    }
}

void mycat2(char* path){
    int file = open(path, O_RDONLY);
    char buffer[MAX_SIZE];
    int bytesRead;
    while((bytesRead = read(file, buffer, MAX_SIZE)) > 0){
        write(1, buffer, bytesRead);
    }
    close(file);
}

// Exercicio 2
int mycp (char* s, char* d){
    int src = open(s, O_RDONLY);
    if (src < 0) {
        perror(s);
        return -1;
    }

    int dest = open(d, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (dest < 0) {
        perror(d);
        return -1;
    }

    char* buffer = (char*) malloc(MAX_SIZE * sizeof(char));
    ssize_t bytesRead, bytesWritten;

    while((bytesRead = read(src, buffer, MAX_SIZE)) > 0){
        bytesWritten = write(dest, buffer, bytesRead);

        if (bytesRead != bytesWritten) return -1;
    }
    free(buffer);

    close(src);
    close(dest);
    return 0;
}

int main(int argc, char* argv[]){
    //Resolução do exercício 1
    //char c[10];
    //int n, fd = 0;
    //if (argc == 2) {
    //    fd = open(argv[1], O_RDONLY);
    //    if (fd == -1) {
    //        perror(argv[1]);
    //        return 1;
    //    }
    //}
    //n = read(0, &c, sizeof(c));
    //if (n > 0) {
    //    write(1, &c, n);
    //    n = read(0, &c, sizeof(c));
    //}
    
    //mycat();
    //mycat2("text.txt");
    mycp(argv[1], argv[2]);
    return 0;
}