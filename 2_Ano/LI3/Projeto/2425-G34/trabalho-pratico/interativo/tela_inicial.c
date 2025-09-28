#include <ncurses.h>
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <stdlib.h>
#include <glib.h>

#include "../include/interativo/queries.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/gestores/gestorArtista.h"
#include "../include/gestores/gestor_albums.h"
#include "../include/gestores/gestor_historico.h"
#include "../include/my_recomendador.h"
#include "../include/Queries.h"
#include "../include/Output.h"
#include "../include/utils.h"
#include "../include/interativo/tela_inicial.h"
#include "../include/interativo/seleciona_dataset.h"
#include "../include/interativo/tela_queries.h"
#include "../include/interativo/executa_query.h"


void tela_inicial() {
    int escolha;
    char *msg = "Bem vindo ao Programa interativo de LI3";
    char *start = " Start ";
    char *leave = " Leave ";
    char *names[] = {"Afonso Martins", "Gonçalo Castro", "Luís Felício"};
    int highlight = 0; 

    int row, col;
    getmaxyx(stdscr, row, col);

    while (1) {
        clear();

        // Exibe a mensagem de boas-vindas
        mvprintw(row / 2 - 1, (col - strlen(msg)) / 2, "%s", msg);

        // Exibe as opções Start e Leave
        mvprintw(row / 2 + 1, (col - strlen(start)) / 2, "[%s]", start);
        mvprintw(row / 2 + 3, (col - strlen(leave)) / 2, "[%s]", leave);

        // Exibe os nomes na parte inferior
        int start_col = (col - (strlen(names[0]) + strlen(names[1]) + strlen(names[2]) + 4)) / 2;
        mvprintw(row - 3, start_col, "%s", names[0]);
        mvprintw(row - 3, start_col + strlen(names[0]) + 2, "%s", names[1]);
        mvprintw(row - 3, start_col + strlen(names[0]) + strlen(names[1]) + 4, "%s", names[2]);

        // Destaque a opção selecionada
        if (highlight == 0) {
            attron(A_REVERSE);
            mvprintw(row / 2 + 1, (col - strlen(start)) / 2, "[%s]", start);
            attroff(A_REVERSE);
        } else {
            attron(A_REVERSE);
            mvprintw(row / 2 + 3, (col - strlen(leave)) / 2, "[%s]", leave);
            attroff(A_REVERSE);
        }

        // Captura a entrada do teclado
        escolha = getch();

        switch (escolha) {
            case KEY_UP:
            case KEY_DOWN:
                highlight = !highlight; // Alterna entre Start e Leave
                break;
            case 10: // ENTER
                if (highlight == 0) {
                    return; // Sai para a próxima etapa
                } else {
                    endwin(); // Finaliza o ncurses
                    exit(0);  // Sai do programa
                }
        }
    }
}