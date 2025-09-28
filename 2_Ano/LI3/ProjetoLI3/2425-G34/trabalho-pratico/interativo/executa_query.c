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

void executar_query(int query_num, GHashTable *utilizador_table, GHashTable *musica_table, GHashTable *artistas_table, GHashTable *albums_table, GHashTable *user_music_table,DiscoArtist **discografyArtists, gint **matriz, gint *idsUtilizadores, char **nomesGenerosPtr, GenreLikes ***gL, GHashTable **cumulativeRanking, GHashTable *historicos_por_user_table) {
    clear();
    mvprintw(2, 2, "Executando Query %d...", query_num);
    refresh();

    // Chama a função associada ao número da query
    switch (query_num) {
        case 1: query1_interativo(utilizador_table,artistas_table,musica_table); break;
        case 2: query2_interativo(discografyArtists,artistas_table); break;
        case 3: query3_interativo(gL); break;
        case 4: query4_interativo(cumulativeRanking,artistas_table); break;
        case 5: query5_interativo(user_music_table, g_hash_table_size(utilizador_table), matriz, utilizador_table, idsUtilizadores, nomesGenerosPtr); break;
        case 6: query6_interativo(historicos_por_user_table,musica_table,artistas_table,albums_table); break;
        default: break;
    }
    refresh();
    getch();
}