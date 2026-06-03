#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <fcntl.h>

int main(){
    int fd = open("fifo", O_RDONLY);
    if (fd == -1) {
        perror("Error opening FIFO");
        return 1;
    }
    char buffer[100];
    int bytesRead;

    while ((bytesRead = read(fd, buffer, sizeof(buffer))) > 0) {
        write(1, buffer, bytesRead);
    }

    return 0;   
}