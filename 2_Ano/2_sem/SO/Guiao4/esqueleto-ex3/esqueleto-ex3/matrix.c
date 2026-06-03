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

void lookupNumber(int** matrix, int value, int* vector){    
    int pd[2];
    pipe(pd);

    for (int i = 0; i < ROWS; i++) {
        pid_t pid = fork();
        if (pid == -1) {
            perror("fork");
            return;
        }
        if (pid == 0) {
            // FILHO
            close (pd[0]);
            int ocur_nr = 0;
            for (int j = 0; j < COLUMNS; j++) {
                if (matrix[i][j] == value) {
                    ocur_nr++;
                }
            }

            Minfo info;
            info.line_nr = i;
            info.ocur_nr = ocur_nr;

            write(pd[1], &info, sizeof(Minfo));
            close(pd[1]);
            _exit(0);
        }
    }

    close (pd[1]);
    for (int i = 0; i < ROWS; i++) {
        Minfo info;
        read(pd[0], &info, sizeof(Minfo));
        vector[info.line_nr] = info.ocur_nr;
    }
    close(pd[0]);
}