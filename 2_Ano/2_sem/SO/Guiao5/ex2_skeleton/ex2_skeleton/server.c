#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include "defs.h"
#include "vector.h"

//FIFO criado pelo servidor
//Cliente pode receber um sigpipe (concorrência!)

int main (int argc, char * argv[]){

	init_vector();
	print_vector();

	int client = open(CLIENT, O_RDONLY);
	int server = open(CLIENT, O_WRONLY);

	Msg msg;
	int bytesRead;
	while ((bytesRead = read(client, &msg, sizeof(msg))) > 0) {
		printf("Msg received: %d\n", msg.needle);

		mkfifo(SERVER, 0666);
		int response = open(SERVER, O_WRONLY);

		int n = count_needle(msg.needle);
		Msg message;
		message.needle = msg.needle;
		message.pid = getpid();
		message.occurrences = n;
		write(response, &message, sizeof(message));
	}
	
	return 0;
}
