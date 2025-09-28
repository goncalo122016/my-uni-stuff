#include "../../include/gestores/gestorArtista.h"

#include <glib.h>
#include <stdint.h>
#include <stdio.h>

#include "../../include/Output.h"
#include "../../include/parsers/parser_artista.h"
#include "../../include/entities/Artista.h"
#include "../../include/entities/Musica.h"
#include "../../include/gestores/gestor_musica.h"
#include "../../include/utils.h"

#define MAX_SIZE 2048

struct DiscoArtist {
  gint discografia;
  gint Id;
};

typedef struct {
  gint artist_id;
  gint music_count;
  gint listening_time;
} ArtistInfo;

DiscoArtist *newDiscoArtist() {
  DiscoArtist *d = malloc(sizeof(DiscoArtist));
  d->Id = 0;
  d->discografia = 0;
  return d;
}

// Função para construir uma Hash Table de artistas a partir de um ficheiro .CSV
GHashTable *build_artist_hash(const gchar *filename) {
  static gboolean header_written =
      FALSE;  // Variável estática para controlar se o cabeçalho já foi escrito

  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    printf("Erro: Não foi possível abrir o ficheiro %s.\n", filename);
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

  GHashTable *artist_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free , (GDestroyNotify)removeArtist);
  gchar line[MAX_SIZE];
  while (fgets(line, sizeof(line), file) != NULL) {
    Artista *artista = process_Artista(line);
    if (artista != NULL) {
      gint *id = g_malloc(sizeof(gint));
      *id = getArtistId(artista);
      g_hash_table_insert(artist_table, id, artista);
    } else {
      // Adicionar o cabeçalho ao ficheiro de erros
      if (!header_written) {
        write_output_file("resultados/artists_errors.csv", header_line);
        header_written = TRUE;
      }
      write_output_file("resultados/artists_errors.csv", line);
    }
  }
  fclose(file);
  return artist_table;
}

gint getDiscografia(DiscoArtist *d) { return d->discografia; }

gint getDiscografiaArtistaId(DiscoArtist *d) { return d->Id; }

void setDiscografia(DiscoArtist *d, gint discografia) {
  d->discografia = discografia;
}

void setArtistaDiscoArtistId(DiscoArtist *d, gint id) { d->Id = id; }

Artista *getArtistaById(GHashTable *artistas, gint id) {
  return (Artista *)g_hash_table_lookup(artistas, &id);
}

// Função para obter os artistas de um país
GHashTable *get_Artists_by_Country(GHashTable *table, gchar *country) {
  GHashTableIter iter;
  gpointer key, value;
  GHashTable *artists =
      g_hash_table_new_full(g_str_hash, g_str_equal, free, NULL);
  g_hash_table_iter_init(&iter, table);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    Artista *a = (Artista *)value;
    gchar *c = getArtistCountry(a);
    if (g_strcmp0(c, country) == 0) {
      gint *id = g_malloc(sizeof(gint));
      *id = getArtistId(a);
      g_hash_table_insert(artists, id, a);
    }
    g_free(c);
  }
  return artists;
}

// Função para libertar a memória de um DiscoArtist
void removeDiscoArtist(DiscoArtist *d) {
  if (!d) return;  // Avoid NULL dereference
  free(d);         // Free the struct itself
}

gint esta_em_algum_grupo(gint id, GHashTable *artistas) {
  gint *ids = NULL;
  gint novo_id = 0;
  for (gint i = 1; i < 6; i++) {
    novo_id = id - i;
    Artista *a =
        (Artista *)g_hash_table_lookup(artistas, &novo_id);
    ids = getArtistIdConstituent(a);
    if (ids != NULL && getNumeroConstituents(a) >= i) {
      g_free(ids);
      return novo_id;
    }
    g_free(ids);
    ids = NULL;
  }
  g_free(ids);
  return 0;
}

void insertionSort(DiscoArtist **arr, gint *n,
                   DiscoArtist *newDisco) {
  gint i = *n - 1;
  while (i >= 0 && (arr[i]->discografia < newDisco->discografia ||
                    (arr[i]->discografia == newDisco->discografia &&
                     (getDiscografiaArtistaId(arr[i]) ==
                      getDiscografiaArtistaId(newDisco))))) {
    arr[i + 1] = arr[i];  // Shift elements to the right
    i--;
  }

  // Insert the new artist at the correct position
  arr[i + 1] = newDisco;
  (*n)++;
}

DiscoArtist **makeDiscografiaArtistas(GHashTable *artistas) {
  DiscoArtist **artists =
      malloc(MAX_ARTISTS * sizeof(DiscoArtist *));
  if (artists) {
    for (int i = 0; i < MAX_ARTISTS; i++) {
      artists[i] = NULL;
    }
  }

  GHashTableIter iter;
  gpointer key, value;
  g_hash_table_iter_init(&iter, artistas);
  gint n = 0;

  while (g_hash_table_iter_next(&iter, &key, &value)) {
    Artista *a = (Artista *)value;
    DiscoArtist *d = newDiscoArtist();
    gint id = getArtistId(a);
    setDiscografia(d, getArtistDisco(a));
    setArtistaDiscoArtistId(d, id);
    insertionSort(artists, &n, d);
  }
  return artists;
}

double makeRevenue(gint id, GHashTable *musicas, GHashTable *artistas) {
  gint reproducaoTotal = 0;
  gint reproducaoTotalGrupo = 0;
  double receitaTotal = 0.00;
  double receitaGrupo = 0.00;

  gint id_grupo = esta_em_algum_grupo(id, artistas);

  GHashTableIter iter;
  gpointer key, value;
  g_hash_table_iter_init(&iter, musicas);

  while (g_hash_table_iter_next(&iter, &key, &value)) {
    Musica *m = (Musica *)value;

    gint *ids_m = getArtistId_m(m);
    gint length = getNumeroArtistas(m);
    for (int i = 0; i < length; i++) {
      if (id == ids_m[i]) {
        reproducaoTotal += getReproducoes(m);
      } else if (id_grupo && ids_m[i] == id_grupo) {
        reproducaoTotalGrupo += getReproducoes(m);
      }
    }
    g_free(ids_m);
  }

  if (reproducaoTotalGrupo) {
    Artista *a =
        (Artista *)g_hash_table_lookup(artistas, &id_grupo);
    receitaGrupo =
        ((double)reproducaoTotalGrupo * getArtistRecipePerStream(a)) /
        getNumeroConstituents(a);
  }

  Artista *art = (Artista *)g_hash_table_lookup(artistas, &id);
  receitaTotal =
      (double)reproducaoTotal * getArtistRecipePerStream(art) + receitaGrupo;

  return receitaTotal;
}

void insertTop10(DiscoArtist *top10[], int *size,
                 DiscoArtist *newArtist) {
  int i;
  if (*size == 0) {
    top10[*size] = newArtist;  // Add new artist
    (*size)++;
    return;
  }
  // If the array is not full, insert the new artist at the end and sort
  gint idArtist = getDiscografiaArtistaId(newArtist);
  gint id = getDiscografiaArtistaId(top10[*size - 1]);
  if (*size < 10) {
    top10[*size] = newArtist;  // Add new artist
    (*size)++;                 // Increment the size
  } else if (getDiscografia(newArtist) > getDiscografia(top10[*size - 1]) ||
             (getDiscografia(newArtist) == getDiscografia(top10[*size - 1]) &&
              idArtist < id)) {
    // If the array is full, replace only if the new artist qualifies
    removeDiscoArtist(top10[*size - 1]);
    top10[*size - 1] = newArtist;
  } else {
    removeDiscoArtist(newArtist);
    return;
  }
  // Sort the array using insertion sort logic
  for (i = *size - 1; i > 0; i--) {
    gint idTemp = getDiscografiaArtistaId(top10[i]),
         idAtual = getDiscografiaArtistaId(top10[i - 1]);
    // Compare based on Discografia first, then on Id if Discografia is equal
    if (getDiscografia(top10[i]) > getDiscografia(top10[i - 1]) ||
        (getDiscografia(top10[i]) == getDiscografia(top10[i - 1]) &&
         idTemp < idAtual)) {
      // Swap elements
      DiscoArtist *temp = top10[i];
      top10[i] = top10[i - 1];
      top10[i - 1] = temp;
    } else {
      break;  // No need to check earlier elements
    }
  }
}

DiscoArtist ***makeWeeklyTop(GHashTable *artistas) {
  DiscoArtist ***array =
      malloc(NUMBER_WEEKS * sizeof(DiscoArtist **));
  for (int i = 0; i < NUMBER_WEEKS; i++) {
    array[i] = malloc(10 * sizeof(DiscoArtist *));
    for (int j = 0; j < 10; j++) {
      array[i][j] = NULL;
    }
  }

  int elementos[327] = {0};
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init(&iter, artistas);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    Artista *a = (Artista *)value;
    for (int i = 0; i < NUMBER_WEEKS; i++) {
      gint discoSemanal = getDiscoSemanal(a, i);
      if (discoSemanal == 0) continue;
      gint id =  getArtistId(a);
      DiscoArtist *d = newDiscoArtist();
      setArtistaDiscoArtistId(d, id);
      setDiscografia(d, discoSemanal);
      insertTop10(array[i], &(elementos[i]), d);
    }
  }
  return array;
}

void freeWeeklyTop(DiscoArtist ***array) {
  if (!array) return;  // Check for NULL to avoid crashes

  for (int i = 0; i < NUMBER_WEEKS; i++) {
    if (array[i]) {
      for (int j = 0; j < 10; j++) {
        if (array[i][j]) {
          removeDiscoArtist(array[i][j]);  // Free individual artists
        }
      }
      free(array[i]);  // Free the inner array
    }
  }

  free(array);  // Free the outer array
}

GHashTable **computeCumulativeRankings(DiscoArtist ***weeklyTop) {
  // Allocate an array of GHashTables for each week's cumulative ranking
  GHashTable **cumulativeRankings = malloc(NUMBER_WEEKS * sizeof(GHashTable *));

  // Initialize hash tables for cumulative rankings
  for (int week = 0; week < NUMBER_WEEKS; week++) {
    cumulativeRankings[week] =
        g_hash_table_new_full(g_int_hash, g_int_equal, free, free);
  }

  // Process weeklyTop to compute cumulative rankings
  for (int week = 0; week < NUMBER_WEEKS; week++) {
    // Copy previous week's data into the current week's hash table
    if (week > 0) {
      GHashTableIter iter;
      gpointer key, value;

      g_hash_table_iter_init(&iter, cumulativeRankings[week - 1]);
      while (g_hash_table_iter_next(&iter, &key, &value)) {
        gint *artistId = malloc(sizeof(gint));
        *artistId = *(gint *)key;
        int *appearances = malloc(sizeof(int));
        *appearances = *(int *)value;
        g_hash_table_insert(cumulativeRankings[week], artistId, appearances);
      }
    }

    // Update the current week's cumulative ranking with this week's top artists
    for (int rank = 0; rank < 10; rank++) {
      if ((weeklyTop[week] == NULL || weeklyTop[week][rank] == NULL)) break;
      gint idArtist = getDiscografiaArtistaId(weeklyTop[week][rank]);

      gint *key = malloc(sizeof(gint));
      *key = idArtist;
      int *count = g_hash_table_lookup(cumulativeRankings[week], key);

      if (!count) {
        count = malloc(sizeof(int));
        *count = 0;
        g_hash_table_insert(cumulativeRankings[week], key, count);
      }
      else g_free(key);

      (*count)++;
    }
  }

  return cumulativeRankings;
}

static gint compare_listening_time(gconstpointer a, gconstpointer b) {
  ArtistInfo *artist_a = *(ArtistInfo **)a;
  ArtistInfo *artist_b = *(ArtistInfo **)b;

  // Comparar listening_time em ordem decrescente
  if (artist_a->listening_time != artist_b->listening_time) {
    return artist_b->listening_time - artist_a->listening_time;
  }

  // Se listening_time for igual, comparar artist_id em ordem crescente
  if (artist_a->artist_id < artist_b->artist_id) {
    return -1;  // artist_a is "less than" artist_b
  } else if (artist_a->artist_id > artist_b->artist_id) {
    return 1;  // artist_a is "greater than" artist_b
  } else {
    return 0;  // artist_a and artist_b are equal
  }
}

gint calcularArtistaFavorito(GHashTable *artist_stats,
                             GHashTable *artistas_table) {
  GPtrArray *sorted_artists = g_ptr_array_new();
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init(&iter, artist_stats);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    g_ptr_array_add(sorted_artists, value);
  }
  g_ptr_array_sort(sorted_artists, (GCompareFunc)compare_listening_time);

  if (sorted_artists->len > 0) {
    ArtistInfo *top_artist = g_ptr_array_index(sorted_artists, 0);
    Artista *a =
        g_hash_table_lookup(artistas_table, &top_artist->artist_id);
    g_ptr_array_free(sorted_artists, TRUE);
    if (a) {
      gint id = getArtistId(a); 
      return id;
    } else {
      return 0;
    }
  }

  g_ptr_array_free(sorted_artists, TRUE);
  return 0;
}

void atualizarEstatisticasArtista(Musica *musica,
                                  GHashTable *artist_stats, gint duracao) {
  gint *artista_ids = getArtistId_m(musica);
  if (artista_ids) {
    for (gint i = 0; i < getNumeroArtistas(musica); i++) {
      gint artista_id = artista_ids[i];
      ArtistInfo *info = g_hash_table_lookup(artist_stats, &artista_id);
      if (info) {
        info->music_count++;
        info->listening_time += duracao;
      } else {
        info = g_new0(ArtistInfo, 1);
        info->artist_id = artista_id;
        info->music_count = 1;
        info->listening_time = duracao;
        g_hash_table_insert(artist_stats, &info->artist_id, info);
      }
    }
    g_free(artista_ids);
  }
}

void mostrarTopNArtistas(GHashTable *artist_stats, GHashTable *artistas_table,
                         gint N, gchar ***output, gint *output_size,
                         gchar separator) {
  GPtrArray *sorted_artists = g_ptr_array_new();
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init(&iter, artist_stats);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    g_ptr_array_add(sorted_artists, value);
  }
  g_ptr_array_sort(sorted_artists, (GCompareFunc)compare_listening_time);

  // Iterar sobre os artistas e adicionar ao output apenas quando encontrado
  for (int i = 0; i < MIN(N, (gint)sorted_artists->len); i++) {
    ArtistInfo *artist = g_ptr_array_index(sorted_artists, i);
    gint music_count = artist->music_count;
    Artista *a = g_hash_table_lookup(artistas_table, &artist->artist_id);
    gint artist_id = a ? getArtistId(a) : 0;
    gchar *listening_time = time_seconds_to_hms(artist->listening_time);

    // Realloca memória para a nova linha no array de output
    (*output_size)++;
    *output = g_realloc(*output, (*output_size) * sizeof(gchar *));
    gchar *zeros = createZerosString(7 - contar_digitos(artist_id));
    (*output)[*output_size - 1] =
        g_strdup_printf("A%s%d%c%d%c%s\n", zeros, artist_id, separator,
                        music_count, separator, listening_time);

    g_free(listening_time);
    g_free(zeros);
  }

  g_ptr_array_free(sorted_artists, TRUE);
}
