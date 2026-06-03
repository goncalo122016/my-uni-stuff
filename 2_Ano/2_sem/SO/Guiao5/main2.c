#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <fcntl.h>

int main(){
    int fd = open("fifo", O_WRONLY);
    if (fd == -1) {
        perror("Error opening FIFO");
        return 1;
    }
    char buffer[100];
    int bytesRead;

    while ((bytesRead = read(0, buffer, sizeof(buffer))) > 0) {
        write(fd, buffer, bytesRead);
    }

    return 0;
}