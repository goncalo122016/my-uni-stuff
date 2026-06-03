#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <glib.h>

#include "../include/Queries.h"

// Exemplo de input: ./programa-principal dataset/sem_erros dataset/inputs_exemplo.txt
int main(int argc, char *argv[]) {
  if (argc != 3) {
    printf("Wrong number of arguments given.\n");
    printf(
        "Usage: %s <dataset_directory> <commands_file_path>\n", argv[0]);
    return -1;
  }

  char *dataset_directory = argv[1];
  char *commands_file_path = argv[2];

  gchar *utilizadores_filename = g_strdup_printf("%s/users.csv", dataset_directory);
  gchar *musica_filename = g_strdup_printf("%s/musics.csv", dataset_directory);
  gchar *artistas_filename = g_strdup_printf("%s/artists.csv", dataset_directory);
  gchar *albuns_filename = g_strdup_printf("%s/albums.csv", dataset_directory);
  gchar *historico_filename = g_strdup_printf("%s/history.csv", dataset_directory);

  process_queries_from_file(commands_file_path, utilizadores_filename, musica_filename, artistas_filename, albuns_filename, historico_filename);

  g_free(utilizadores_filename);
  g_free(musica_filename);
  g_free(artistas_filename);
  g_free(albuns_filename);
  g_free(historico_filename);

  return 0;
}