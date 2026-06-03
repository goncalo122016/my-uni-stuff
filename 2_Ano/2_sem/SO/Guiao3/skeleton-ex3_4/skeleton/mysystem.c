#include "mysystem.h"


// recebe um comando por argumento
// returna -1 se o fork falhar
// caso contrario retorna o valor do comando executado
int mysystem (char* command) {
	char* cmd;
    char* tokens[10];
    int i = 0, res = -1;

    cmd = strtok(command, " ");
    while (cmd != NULL && i < 10) {
        tokens[i++] = cmd;
        cmd = strtok(NULL, " ");
    }

    tokens[i] = NULL;

	pid_t pid = fork();

	if (pid == -1) {
		perror("fork");
		return -1;
	} else if (pid == 0) {
		int r = execvp(tokens[0], tokens);
		perror("Erro");
		_exit(r);
	} else {
		int status;
		wait(&status);
		if (WIFEXITED(status)) {
			//printf("[PAI]: O filho %d terminou com o código %d\n", pid, WEXITSTATUS(status));
			res = WEXITSTATUS(status);
			if (res == 255) res = -1;
		}
	}

	return res;
}