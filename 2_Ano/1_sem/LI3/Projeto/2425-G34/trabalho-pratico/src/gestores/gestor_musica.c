#include <stdio.h>
#include <glib.h>

#include "../../include/gestores/gestorUtilizador.h"
#include "../../include/gestores/gestor_musica.h"
#include "../../include/entities/Utilizador.h"
#include "../../include/parsers/parser_musica.h"
#include "../include/Output.h"
#include "../include/utils.h"

#define MAX_SIZE 2048

struct GenreLikes {
  gchar *genre;
  gint likes;
};

struct UserAgeTuple {
  gchar *user;
  gint age;
};

// Função para procurar um utilizador específico pelo Username(Id) a partir de
// uma Hash Table de utilizadores
GHashTable *build_musica_hash_table(const gchar *filename, GHashTable *artistas_table, GHashTable *albuns_table) {
  static gboolean header_written = FALSE;  // Variável estática para controlar se o cabeçalho já foi escrito

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

  // IGNORAR a primeira linha (cabeçalho) do ficheiro
  gchar header_line[MAX_SIZE];
  if (fgets(header_line, sizeof(header_line), file) == NULL) {
    printf(
        "Erro: O ficheiro está vazio ou ocorreu um problema ao ler o "
        "cabeçalho.\n");
    fclose(file);
    return NULL;
  }

  GHashTable *musica_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)destroyMusica);

  gchar line[MAX_SIZE];
  while (fgets(line, sizeof(line), file) != NULL) {
    line[strcspn(line, "\n")] = 0;

    Musica *musica = parse_Musica(line,artistas_table, albuns_table);

    if (musica != NULL) {
      gint *id = g_malloc(sizeof(gint));
      *id = getId(musica);
      g_hash_table_insert(musica_table, id, musica);
    } else {
      // Adicionar o cabeçalho ao ficheiro de erros
      if (!header_written) {
        write_output_file("resultados/musics_errors.csv", header_line);
        header_written = TRUE;
      }
      line[strcspn(line, "\0")] = '\n';
      write_output_file("resultados/musics_errors.csv", line);
    }
  }
  fclose(file);
  return musica_table;
}


GenreLikes ***createGenreLikesTotal() {
  GenreLikes ***gL = malloc(sizeof(GenreLikes **) * 44);
  for (int i = 0; i < 44; i++) {
    gL[i] = malloc(sizeof(GenreLikes *) * 10);
    for (int j = 0; j < 10; j++) {
      gL[i][j] = malloc(sizeof(GenreLikes));
      gL[i][j]->likes = 0;
      gL[i][j]->genre = NULL;
    }
  }
  return gL;
}

GenreLikes *createGenreLikes(){
  GenreLikes *gL = malloc(sizeof(GenreLikes));
  gL->genre = NULL;
  gL->likes = 0;
  return gL;
}

void freeGenreLikesTotal(GenreLikes ***gL) {
    if (gL == NULL) return;

    for (int i = 0; i < 44; i++) {
        if (gL[i] != NULL) {
            for (int j = 0; j < 10; j++) {
                if (gL[i][j] != NULL) {
                    free(gL[i][j]->genre); // Free genre string if allocated
                    free(gL[i][j]);
                }
            }
            free(gL[i]);
        }
    }
    free(gL);
}

gint getLikes_gL(GenreLikes *gL){
  return gL->likes;
}

void aumentaLikes(GenreLikes *gL, gint aumento){
  gL->likes += aumento;
}

void setGenre_gL(GenreLikes *gL, gchar *genre){
  g_free(gL->genre);
  gL->genre = g_strdup(genre);
}

gchar *getGenre_gL(GenreLikes *gL){
  return g_strdup(gL->genre);

}

int compareGenrePopularity(const void *a, const void *b) {
    GenreLikes *g1 = *(GenreLikes **)a;
    GenreLikes *g2 = *(GenreLikes **)b;

    // Comparação por likes (decrescente)
    if (g2->likes != g1->likes) {
        return g2->likes - g1->likes;
    }

    // Comparação alfabética (crescente)
    return g_strcmp0(g1->genre, g2->genre);
}

void destroyGenreLike(GenreLikes *gL){
  if(gL){
    g_free(gL->genre);
    free(gL);
  }
}

gchar *calcularGeneroMaisOuvido(GHashTable *genre_stats) {
    GHashTableIter iter;
    gpointer key, value;
    gint max_listening_time = -1;
    gchar *most_listened_genre = NULL;

    g_hash_table_iter_init(&iter, genre_stats);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        gchar *genre = (gchar *)key;
        gint *listening_time = (gint *)value;

        if (*listening_time > max_listening_time ||
            (*listening_time == max_listening_time && g_strcmp0(genre, most_listened_genre) < 0)) {
            most_listened_genre = genre;
            max_listening_time = *listening_time;
        }
    }

    if (most_listened_genre) {
        return g_strdup(most_listened_genre);
    } else {
        return NULL;
    }
}

void atualizarEstatisticasGenero(Musica *musica, GHashTable *genre_stats, gint duracao) {
    gchar *genre = getGenre(musica);
    if (genre) {
        gint *genre_time = g_hash_table_lookup(genre_stats, genre);
        if (genre_time) {
            *genre_time += duracao;
        } else {
            genre_time = g_new0(gint, 1);
            *genre_time = duracao;
            g_hash_table_insert(genre_stats, g_strdup(genre), genre_time);
        }
        g_free(genre);
    }
}

gchar *get_genre_by_id(GHashTable *musicas, gint *id) {
  Musica *music = (Musica *)g_hash_table_lookup(musicas, id);
  if (music) {
    return getGenre(music);
  }
  return NULL;
}

