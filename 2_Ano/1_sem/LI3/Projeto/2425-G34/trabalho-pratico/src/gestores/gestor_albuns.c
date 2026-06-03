#include <glib.h>
#include <stdint.h>
#include <stdio.h>

#include "../../include/Output.h"
#include "../../include/parsers/parser_albuns.h"
#include "../../include/entities/Albums.h"
#include "../../include/entities/Musica.h"
#include "../../include/entities/Utilizador.h"
#include "../../include/gestores/gestorArtista.h"

#define MAX_SIZE 1024

GHashTable *build_albums_hash(const gchar *filename,
                              GHashTable *artistas_table) {
  static gboolean header_written = FALSE;

  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    printf("Erro: Não foi possível abrir o ficheiro %s.\n", filename);
    return NULL;
  }

  if (artistas_table == NULL) {
    printf("Erro: Tabela de artistas não está inicializada.\n");
    fclose(file);
    return NULL;
  }

  gchar header_line[MAX_SIZE];
  if (fgets(header_line, sizeof(header_line), file) == NULL) {
    printf(
        "Erro: O ficheiro está vazio ou ocorreu um problema ao ler o "
        "cabeçalho.\n");
    fclose(file);
    return NULL;
  }

  // Create hash table with appropriate destroy functions
  GHashTable *albums_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)deleteAlbum);

  gchar line[MAX_SIZE];
  while (fgets(line, sizeof(line), file) != NULL) {
    Album *a = process_Albums(line, artistas_table);
    if (a != NULL) {
      gint *id = g_malloc(sizeof(gint));
      *id = Album_getId(a);
      g_hash_table_insert(albums_table, id, a);
    } else {
      if (!header_written) {
        write_output_file("resultados/albums_errors.csv", header_line);
        header_written = TRUE;
      }
      // Make sure we have a newline
      size_t len = strlen(line);
      if (len > 0 && line[len - 1] != '\n') {
        line[len] = '\n';
        line[len + 1] = '\0';
      }
      write_output_file("resultados/albums_errors.csv", line);
    }
  }
  fclose(file);
  return albums_table;
}

gint calcularAlbumMaisOuvido(GHashTable *album_stats,
                             GHashTable *albuns_table) {
  GHashTableIter iter;
  gpointer key, value;
  gint max_album_time = -1;
  gint most_listened_album_id = 0;

  g_hash_table_iter_init(&iter, album_stats);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    gint *listening_time = (gint *)value;
    if (*listening_time > max_album_time ||
        (*listening_time == max_album_time &&
         (gint)(intptr_t)key < most_listened_album_id)) {
      most_listened_album_id = *(gint *)key;
      max_album_time = *listening_time;
    }
  }

  if (most_listened_album_id != 0) {
    Album *most_listened_album =
        g_hash_table_lookup(albuns_table, &most_listened_album_id);
    if (most_listened_album) {
      gint id = Album_getId(most_listened_album);
      return id;
    } else {
      return 0;
    }
  }
  return 0;
}

void atualizarEstatisticasAlbum(Musica *musica, GHashTable *album_stats,
                                gint duracao) {
  gint *album_id = malloc(sizeof(gint));
  *album_id = getAlbumId_m(musica);
  if (*album_id) {
    gint *album_time = g_hash_table_lookup(album_stats, album_id);
    if (album_time) {
      g_free(album_id);
      *album_time += duracao;
    } else {
      album_time = g_new0(gint, 1);
      *album_time = duracao;
      g_hash_table_insert(album_stats, album_id, album_time);
    }
  }
  else g_free(album_id);
}