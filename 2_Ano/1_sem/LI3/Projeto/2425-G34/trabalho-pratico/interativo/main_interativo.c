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


#define MAX_SIZE 1024

int main() {
    initscr();
    echo();
    curs_set(FALSE);
    keypad(stdscr, TRUE);


    tela_inicial();
    
    char *dataset_directory = NULL;
    char *commands_file_path = NULL;
    selecionar_dataset(&dataset_directory, &commands_file_path);

    gchar *utilizadores_filename = g_strdup_printf("%s/users.csv", dataset_directory);
    gchar *musica_filename = g_strdup_printf("%s/musics.csv", dataset_directory);
    gchar *artistas_filename = g_strdup_printf("%s/artists.csv", dataset_directory);
    gchar *albuns_filename = g_strdup_printf("%s/albums.csv", dataset_directory);
    gchar *historico_filename = g_strdup_printf("%s/history.csv", dataset_directory);

    GHashTable *artistas_table = build_artist_hash(artistas_filename);
  if (artistas_table == NULL) {
    printf("Erro ao construir a tabela de artistas.\n");
  }

  GHashTable *albums_table = 
      build_albums_hash(albuns_filename, artistas_table);
  if (albums_table == NULL) {
    printf("Erro ao construir a tabela de albuns.\n");
  }
  
  GHashTable *musica_table = build_musica_hash_table(musica_filename, artistas_table,albums_table);
  if (musica_table == NULL) {
    printf("Erro ao construir a tabela de musicas.\n");
  }

  GenreLikes ***gL = createGenreLikesTotal();
  GHashTable *user_music_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)destroy_user_musicas);

  GHashTable *utilizador_table =
      build_utilizador_hash_table(utilizadores_filename, musica_table, gL);
  if (utilizador_table == NULL) {
    printf("Erro ao construir a tabela de utilizadores.\n");
  }

  GHashTable *historicos_por_user_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)g_ptr_array_unref);

  GHashTable *historico_table = build_historico_hash_table(
      historico_filename, utilizador_table, musica_table, user_music_table,
      historicos_por_user_table, artistas_table);
    
  if (historico_table == NULL) {
    printf("Erro ao construir a tabela de historico.\n");
  }


  DiscoArtist **discografyArtists =
      makeDiscografiaArtistas(artistas_table);

  DiscoArtist ***weeklyTop = makeWeeklyTop(artistas_table);
  GHashTable **cumulativeRanking = computeCumulativeRankings(weeklyTop);

      gint** matriz = makeMatrix(utilizador_table,10);

      const char *nomesGeneros[10] = {
      "Rock",   "Pop",   "Country", "Jazz",      "Blues",
      "Reggae", "Metal", "Hip Hop", "Classical", "Electronic"};


    int tamanho_hash_utilizador = g_hash_table_size(utilizador_table);

    gint *idsUtilizadores = malloc((tamanho_hash_utilizador + 1) * sizeof(gint));
    if (!idsUtilizadores) {
        printf("[ERRO] Falha ao alocar memória para idsUtilizadores\n");
    }

    char *nomesGenerosPtr[10];
    for (int i = 0; i < 10; i++) {
        nomesGenerosPtr[i] = (char *)nomesGeneros[i]; 
    }

    fillMatrixWithUserLikes(user_music_table, musica_table,utilizador_table, 
                            matriz, idsUtilizadores, tamanho_hash_utilizador, 
                            nomesGenerosPtr, 10);

    g_free(utilizadores_filename);
    g_free(musica_filename);
    g_free(artistas_filename);
    g_free(albuns_filename);
    g_free(historico_filename);
    free(commands_file_path);

    tela_queries(utilizador_table, musica_table, artistas_table, albums_table, user_music_table, discografyArtists, matriz, idsUtilizadores, nomesGenerosPtr, gL,cumulativeRanking, historicos_por_user_table);

    endwin();
    return 0;
}