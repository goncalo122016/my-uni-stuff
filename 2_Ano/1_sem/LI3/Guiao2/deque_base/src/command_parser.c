#include <stdio.h>
#include <string.h>
#include "command_parser.h"
#include "../include/deque.h";

// Função auxiliar para contar quantos números (inteiros) existem numa linha após o comando
int countArgs(char* line) {
    int count = 0;
    char* token = strtok(NULL, " ");
    while (token != NULL) {
        count++;
        token = strtok(NULL, " ");
    }
    return count;
}

// Função auxiliar para extrair os números da linha e guardá-los no Cmd
void extractArgs(Cmd* cmd, char* line) {
    char* token = strtok(NULL, " ");
    for (int i = 0; i < cmd->nargs; i++) {
        if (token != NULL) {
            cmd->args[i] = atoi(token); // Converte a string para inteiro
            token = strtok(NULL, " ");
        }
    }
}

// Função para analisar uma linha e criar um comando Cmd
Cmd* parseLine(char* line) {
    Cmd* cmd = (Cmd*)malloc(sizeof(Cmd));

    // Extrai o comando (primeira parte da linha)
    char* token = strtok(line, " ");
    cmd->command = strdup(token); // Duplica o comando para a estrutura Cmd

    // Conta o número de argumentos (inteiros) que seguem o comando
    cmd->nargs = countArgs(line);

    // Se houver argumentos, aloca espaço para eles
    if (cmd->nargs > 0) {
        cmd->args = (int*)malloc(cmd->nargs * sizeof(int));
        extractArgs(cmd, line);
    } else {
        cmd->args = NULL;
    }

    return cmd;
}

// Função para processar os comandos
void processCommand(Deque* deque, Cmd* cmd) {
    if (strcmp(cmd->command, "PUSH") == 0) {
        for (int i = 0; i < cmd->nargs; i++) {
            deque_push_back(deque, cmd->args[i]);
        }
    } else if (strcmp(cmd->command, "PUSH FRONT") == 0) {
        for (int i = 0; i < cmd->nargs; i++) {
            deque_push_front(deque, cmd->args[i]);
        }
    } else if (strcmp(cmd->command, "POP") == 0) {
        if (size(deque) > 0) {
            int value = deque_pop_back(deque);
            printf("%d\n", value);
        } else {
            printf("EMPTY\n");
        }
    } else if (strcmp(cmd->command, "POP FRONT") == 0) {
        if (size(deque) > 0) {
            int value = deque_pop_front(deque);
            printf("%d\n", value);
        } else {
            printf("EMPTY\n");
        }
    } else if (strcmp(cmd->command, "SIZE") == 0) {
        printf("%d\n", size(deque));
    } else if (strcmp(cmd->command, "REVERSE") == 0) {
        deque_reverse(deque);
    } else if (strcmp(cmd->command, "PRINT") == 0) {
        printDeque(deque);
    }
}
