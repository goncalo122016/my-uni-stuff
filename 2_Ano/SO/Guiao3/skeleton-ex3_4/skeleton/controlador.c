#include <stdio.h>
#include "mysystem.h"

void controller(int N, char** commands) {
	pid_t pids[N];
	for(int i=0; i < N; i++){
		pid_t pid = fork();
		if(pid == 0){
			int count = 1;
			while (mysystem(commands[i]) != 0) count++;
			_exit(count);
		}
		pids[i] = pid;
	}
	printf("\n");
	for(int i=0; i < N; i++){
		int status;
		waitpid(pids[i],&status,0);
		if(WIFEXITED(status)){
			printf("[PAI]: %s %d\n", commands[i], WEXITSTATUS(status));
		}
	}
}

int main(int argc, char* argv[]) {

    char *commands[argc-1];
    int N = 0;
	for(int i=1; i < argc; i++){
		commands[N] = strdup(argv[i]);
		printf("command[%d] = %s\n", N, commands[N]);
        N++;
	}

    controller(N, commands);

	return 0;
}