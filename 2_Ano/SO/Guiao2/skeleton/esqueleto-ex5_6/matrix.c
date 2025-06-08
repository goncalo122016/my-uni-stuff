#include "matrix.h"


int **createMatrix() {

    // seed random numbers
    srand(time(NULL));

    // Allocate and populate matrix with random numbers.
    printf("Generating numbers from 0 to %d...", MAX_RAND);
    int **matrix = (int **) malloc(sizeof(int*) * ROWS);
    for (int i = 0; i < ROWS; i++) {
        matrix[i] = (int*) malloc(sizeof(int) * COLUMNS);
        for (int j = 0; j < COLUMNS; j++) {
            matrix[i][j] = rand() % MAX_RAND;
        }
    }
    printf("Done.\n");

    return matrix;
}

void printMatrix(int **matrix) {

    for (int i = 0; i < ROWS; i++) {
        printf("%2d | ", i);
        for (int j = 0; j < COLUMNS; j++) {
            printf("%7d ", matrix[i][j]);
        }
        printf("\n");
    }
}

// ex.5
int valueExists(int **matrix, int value) {
    int status;
    int found = 0;
    
    for (int i = 0; i < ROWS; i++) {
        pid_t pid = fork();
        if (pid == 0) {
            for (int j = 0; j < COLUMNS; j++) {
                if (matrix[i][j] == value) {
                    _exit(i);
                }
            }
            _exit(-1);
        }
    }

    while (wait(&status) > 0) {
        if (WEXITSTATUS(status) != 255) { // O valor de retorno é -1 mas o WEXITSTATUS devolve 255
            printf("O numero existe na matriz na linha %d\n", WEXITSTATUS(status));
            found = 1;
        }
    }
    
    return found;
}


// ex.6
void linesWithValue(int **matrix, int value) {
    int status;
    pid_t pid[ROWS];
    
    for (int i = 0; i < ROWS; i++) {
        pid_t pid_aux = fork();
        
        if (pid_aux == 0) {
            for (int j = 0; j < COLUMNS; j++) {
                if (matrix[i][j] == value) {
                    _exit(i);
                }
            }
            _exit(-1);
        }
        pid[i] = pid_aux;
        printf("[Pai]: criei o filho: %d\n", pid_aux);
    }

    for (int i = 0; i < ROWS; i++) {
        pid_t terminated_pid = waitpid(pid[i], &status, 0);

        if (WIFEXITED(status)) {
            printf("O numero existe na matriz na linha %d, do filho: %d\n", WEXITSTATUS(status), terminated_pid);
        }
        else {
            printf("ERRO\n");
        }
    }
}