#include "matrix.h"


int main(int argc, char *argv[]) {

    // generate random matrix
    int **matrix = createMatrix();

    // print matrix
    printMatrix(matrix);

    // search for value
    int value = 0;
    printf("Enter value to search: ");
    scanf("%d", &value);

    if (valueExists(matrix, value)) {
        printf("Value exists in matrix.\n");
    } else {
        printf("Value does not exist in matrix.\n");
    }

    linesWithValue(matrix, value);

    // free matrix
    for (int i = 0; i < ROWS; i++) {
        free(matrix[i]);
    }
    free(matrix);

    return 0;
}