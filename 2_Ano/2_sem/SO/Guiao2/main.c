#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <sys/wait.h> /* chamadas wait*() e macros relacionadas */
#include <stdio.h>
#include <stdlib.h>

int existeNaMatriz(int** matriz, int N, int M, int num) {
    int status;
    int found = 0;
    
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < M; j++) {
            pid_t pid = fork();
            
            if (pid == 0) {
                if (matriz[i][j] == num) {
                    _exit(i);
                }
                _exit(0);
            }
        }
    }

    while (wait(&status) > 0) {
        if (WEXITSTATUS(status) != 0) {
            printf("O numero existe na matriz na linha %d\n", WEXITSTATUS(status));
            found = 1;
        }
    }
    
    return found;
}

void randomMatriz(int** matriz, int N, int M) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < M; j++) {
            matriz[i][j] = rand() % 100;
        }
    }
}

int main(){
    printf("Escolha o exercicio: ");
    int exer = 0;
    scanf("%d", &exer);

    int status;

    switch (exer){
        case 1:
            pid_t pid = getpid();
            pid_t pidPai = getppid();
            printf("PID: %d\n", pid);
            printf("PID Pai: %d\n", pidPai);

            fork();
            printf("Hello\n");
            fork();
            printf("World\n");
            break;

        case 2:
            pid_t pid2 = fork();

            if (pid2 == 0){
                // Filho
                pid_t pidFilho = getpid();
                pid_t pidPai = getppid();
                printf("[FILHO]: PID: %d\n", pidFilho);
                printf("[FILHO]: PID Pai: %d\n", pidPai);

                sleep(5);

                _exit(0);
            }
            else{
                // Pai
                pid_t pidPai = getpid();
                pid_t pidFilho = getppid();
                printf("[PAI]: PID: %d\n", pidPai);
                printf("[PAI]: PID Filho: %d\n", pidFilho);

                pid_t terminated_pid = wait(&status);

                if (WIFEXITED(status)){
                    printf("[PAI]: Terminated normally with the value %d from %d\n", WEXITSTATUS(status), terminated_pid);
                }
                else{
                    printf("[PAI]: ERRO\n");
                }
            }
            break;

        case 3:
            for (int i = 1; i <= 10; i++){
                pid_t pid = fork();
                if (pid == 0){
                    // Filho
                    pid_t pidFilho = getpid();
                    printf("[FILHO]: PID %d: %d\n",i , pidFilho);
                    exit(i);
                }
                else{
                    // Pai
                    pid_t terminated_pid = wait(&status);
                    if (WIFEXITED(status)){
                        printf("[PAI]: Terminated normally with the value %d from %d\n", WEXITSTATUS(status), terminated_pid);
                    }
                    else{
                        printf("[PAI]: ERRO\n");
                    }
                }
            }
            break;

        case 4:
            for (int i = 1; i <= 10; i++){
                pid_t pid = fork();
                if (pid == 0){
                    // Filho
                    pid_t pidFilho = getpid();
                    printf("[FILHO]: PID %d: %d\n",i , pidFilho);
                    _exit(i);
                }
            }

            for (int i = 1; i <= 10; i++){
                pid_t terminated_pid = wait(&status);
                if (WIFEXITED(status)){
                    printf("[PAI]: Terminated normally with the value %d from %d\n", WEXITSTATUS(status), terminated_pid);
                }
                else{
                    printf("[PAI]: ERRO\n");
                }
            }
            break;

        case 5: // Inclui o Exercício 6 também
            int N, M, num;
            printf("Insira o numero de linhas: ");
            scanf("%d", &N);
            printf("Insira o numero de colunas: ");
            scanf("%d", &M);
            printf("Insira o numero a procurar: ");
            scanf("%d", &num);

            int** matriz = (int**) malloc(N * sizeof(int*));
            for (int i = 0; i < N; i++) {
                matriz[i] = (int*) malloc(M * sizeof(int));
            }
            randomMatriz(matriz, N, M);

            // Print da matriz
            for (int i = 0; i < N; i++) {
                for (int j = 0; j < M; j++) {
                    printf("%d ", matriz[i][j]);
                }
                printf("\n");
            }

            if (existeNaMatriz(matriz, N, M, num)){
                printf("O numero %d existe na matriz\n", num);
            }
            else{
                printf("O numero %d não existe na matriz\n", num);
            }

            for (int i = 0; i < 1000; i++) {
                free(matriz[i]);
            }
            free(matriz);
            break;

        default:
            printf("Exercicio não existe!!\n");
            break;
    }

    return 0;
}