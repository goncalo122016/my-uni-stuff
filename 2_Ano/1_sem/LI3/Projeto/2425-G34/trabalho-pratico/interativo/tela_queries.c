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


void tela_queries(GHashTable *utilizador_table, GHashTable *musica_table, GHashTable *artistas_table, GHashTable *albums_table, GHashTable *user_music_table, DiscoArtist **discografyArtists, gint **matriz, gint *idsUtilizadores, char **nomesGenerosPtr,GenreLikes ***gL,GHashTable **cumulativeRanking, GHashTable *historicos_por_user_table) {
    int highlight = 0; 
    int escolha;
    char *opcoes[] = {
        "Query 1",
        "Query 2",
        "Query 3",
        "Query 4",
        "Query 5",
        "Query 6",
        "Sair"
    };
    int num_opcoes = sizeof(opcoes) / sizeof(opcoes[0]);

    while (1) {
        clear();
        for (int i = 0; i < num_opcoes; i++) {
            if (i == highlight) {
                attron(A_REVERSE); 
            }
            mvprintw(i + 2, 2, "%s", opcoes[i]);
            if (i == highlight) {
                attroff(A_REVERSE); 
            }
        }
        escolha = getch();

        switch (escolha) {
            case KEY_UP:
                highlight = (highlight - 1 + num_opcoes) % num_opcoes;
                break;
            case KEY_DOWN:
                highlight = (highlight + 1) % num_opcoes;
                break;
            case 10: // ENTER
                if (highlight == num_opcoes - 1) {
                    mvprintw(num_opcoes + 4, 2, "Obrigado por usar o programa! Pressione qualquer tecla para sair.");
                    refresh();
                    getch();
                    return; // Sai do programa
                }
                else {
                    executar_query(highlight + 1, utilizador_table, musica_table, artistas_table, albums_table, user_music_table, discografyArtists, matriz, idsUtilizadores, nomesGenerosPtr, gL, cumulativeRanking, historicos_por_user_table);
                }
                break;
        }
    }
}