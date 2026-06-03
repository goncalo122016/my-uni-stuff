#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>

int main(){
    mkfifo("fifo", 0666);
    return 0;
}