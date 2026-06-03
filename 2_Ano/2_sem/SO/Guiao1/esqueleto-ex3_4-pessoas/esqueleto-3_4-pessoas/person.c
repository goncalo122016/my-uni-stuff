#include "person.h"
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

#define FILENAME "file_person"

int insertPerson(char* name, int age){
    int f = open(FILENAME, O_CREAT | O_WRONLY | O_APPEND, 0600);
    if (f < 0) {
        perror("erro ao abrir o ficheiro");
        return -1;
    }

    Person p;
    strcpy(p.name, name);
    p.age = age;

    write(f, &p, sizeof(Person));
    close(f);
    return 0;
}

int listPersons(int N){
    int fd = open(FILENAME, O_RDONLY);
    if (fd < 0) {
        perror("erro ao abrir o ficheiro");
        return -1;
    }

    Person p;
    int i = 0;
    while (i < N && read(fd, &p, sizeof(Person)) > 0) {
        char res[256];
        int size = sprintf(res, "Name: %s\nAge: %d\n", p.name, p.age);
        write(1, res, size);
        i++;
    }

    close(fd);
    return 0;
}

int changeAge(char* name, int age){
    int fd = open(FILENAME, O_RDWR);
    if (fd < 0) {
        perror("erro ao abrir o ficheiro");
        return -1;
    }

    Person p;
    int found = 0;
    while (read(fd, &p, sizeof(Person)) > 0 && !found) {
        if (strcmp(p.name, name) == 0) {
            p.age = age;
            int r = lseek(fd, -sizeof(Person), SEEK_CUR);
            if(r < 0) {
                perror("erro ao fazer lseek");
                return -1;
            }
            write(fd, &p, sizeof(Person));
            found = 1;
            break;
        }
    }

    return 0;
}