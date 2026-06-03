#ifndef COMMAND_PARSER_H
#define COMMAND_PARSER_H

#include "deque.h"
#include <string.h>

typedef struct cmd {
    char* command;
    int* args; // NULL se não houver argumentos
    int nargs; // Número de argumentos
} Cmd;

// Função para processar o comando na Deque
void processCommand(Deque* deque, Cmd* cmd);

// Função para analisar uma linha e criar um comando
Cmd* parseLine(char* line);

#endif
