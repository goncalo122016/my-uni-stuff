#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include "defs.h"

int main (int argc, char * argv[]){

	if (argc < 2) {
		printf("Missing argument.\n");
		_exit(1);
	}

	mkfifo(CLIENT, 0666);

	int send = open(CLIENT, O_WRONLY);

	Msg message;
	message.needle = atoi(argv[1]);
	message.pid = getpid();
	message.occurrences = 0;

	write(send, &message, sizeof(message));

	int response = open(SERVER, O_RDONLY);

	Msg result;
	read(response, &result, sizeof(result));
	
	printf("Msg received: %d\n", result.occurrences);
	
	return 0;
}

