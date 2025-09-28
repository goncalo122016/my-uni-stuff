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

void selecionar_dataset(char **dataset_directory, char **commands_file_path) {
    const char *directories[] = {"big", "small"};
    const int num_directories = 2;

    const char *types[] = {"com erros", "sem erros"};
    const int num_types = 2;

    int highlight = 0;
    int choice = 0;

    // Seleção do diretório
    clear();
    mvprintw(2, 2, "Selecione o diretório do dataset:");
    curs_set(FALSE);

    while (1) {
        for (int i = 0; i < num_directories; i++) {
            if (i == highlight) {
                attron(A_REVERSE);
                mvprintw(4 + i, 4, "%s", directories[i]);
                attroff(A_REVERSE);
            } else {
                mvprintw(4 + i, 4, "%s", directories[i]);
            }
        }
        int c = getch();
        switch (c) {
            case KEY_UP:
                highlight = (highlight - 1 + num_directories) % num_directories;
                break;
            case KEY_DOWN:
                highlight = (highlight + 1) % num_directories;
                break;
            case 10: // ENTER
                choice = highlight;
                goto select_type;
        }
    }
    const char *selected_directory;

select_type:
    // Salvar o diretório selecionado
    selected_directory = directories[choice];

    // Seleção do tipo de dataset
    highlight = 0;
    clear();
    mvprintw(2, 2, "Selecione o tipo de dataset:");
    while (1) {
        for (int i = 0; i < num_types; i++) {
            if (i == highlight) {
                attron(A_REVERSE);
                mvprintw(4 + i, 4, "%s", types[i]);
                attroff(A_REVERSE);
            } else {
                mvprintw(4 + i, 4, "%s", types[i]);
            }
        }
        int c = getch();
        switch (c) {
            case KEY_UP:
                highlight = (highlight - 1 + num_types) % num_types;
                break;
            case KEY_DOWN:
                highlight = (highlight + 1) % num_types;
                break;
            case 10: // ENTER
                choice = highlight;
                goto configure_paths;
        }
    }

configure_paths:
    // Configurar o caminho do dataset e do arquivo de comandos com base nas escolhas
    if (strcmp(selected_directory, "big") == 0) {
        if (choice == 0) { // "com erros"
            *dataset_directory = g_strdup("big/dataset/big_com_erros");
        } else { // "sem erros"
            *dataset_directory = g_strdup("big/dataset/big_sem_erros");
        }
        *commands_file_path = g_strdup("big/inputs-big.txt");
    } else if (strcmp(selected_directory, "small") == 0) {
        if (choice == 0) { // "com erros"
            *dataset_directory = g_strdup("small/dataset/com_erros");
        } else { // "sem erros"
            *dataset_directory = g_strdup("small/dataset/sem_erros");
        }
        *commands_file_path = g_strdup("small/inputs-small.txt");
    }

    // Mensagem final
    clear();
    mvprintw(6, 2, "Dataset e arquivo de comandos configurados:");
    mvprintw(8, 2, "Dataset: %s", *dataset_directory);
    mvprintw(9, 2, "Arquivo de comandos: %s", *commands_file_path);
    mvprintw(11, 2, "Pressione qualquer tecla para continuar.");
    refresh();
    getch();
}
