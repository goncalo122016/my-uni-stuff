#include <unistd.h>
#include <stdio.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


#define MAX_COMMANDS 10

// parse the argument and execvp
// returns the result of exec
int exec_command(char* arg){

	//We are assuming a maximum number of arguments of 10.
	//This could be improved with realloc, for example.
	char *exec_args[10];
	int args_count = 0;
	int exec_ret = 0;

	char *token, *string, *tofree;

	tofree = string = strdup(arg);
	assert(string != NULL);

	while ((token = strsep(&string, " ")) != NULL) {
		exec_args[args_count]=token;
		args_count++;
	}
	exec_args[args_count]=NULL;

	exec_ret=execvp(exec_args[0],exec_args);

	perror("Exec error");

    free(tofree);

	return exec_ret;
}


int main(int argc, char** argv){

	int number_of_commands=4;
	char * commands[] = {
		"grep -v ^# /etc/passwd",
		"cut -f7 -d:",
		"uniq",
		"wc -l"
	};

	int fildes[number_of_commands-1][2];

	for (int i=0; i<number_of_commands; i++){
		if (i == 0) {
			// First command
			pipe(fildes[0]);

			pid_t pid = fork();
			if (pid == 0) {
				// FILHO
				close(fildes[0][0]);

				dup2(fildes[0][1], 1);
				close(fildes[0][1]);

				exec_command(commands[0]);
			}
			close(fildes[0][1]);
		} else if (i == number_of_commands - 1) {
			// Last command
			pid_t pid = fork();
			if (pid == 0) {
				// FILHO
				dup2(fildes[i-1][0], 0);
				close(fildes[i-1][0]);

				exec_command(commands[i]);
			}
			close(fildes[i-1][0]);
		}
		else {
			// Middle commands
			pipe(fildes[i]);
			pid_t pid = fork();
			if (pid == 0) {
				// FILHO
				close(fildes[i][0]);

				dup2(fildes[i-1][0], 0);
				close(fildes[i-1][0]);

				dup2(fildes[i][1], 1);
				close(fildes[i][1]);

				exec_command(commands[i]);
			}
			close(fildes[i-1][0]);
			close(fildes[i][1]);
		}
	}

	return 0;
}
