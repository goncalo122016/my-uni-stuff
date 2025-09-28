#include <ctype.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../include/Output.h"
#include "../include/entities/Artista.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/utils.h"

// Função para converter duração em segundos
gint convert_duration_to_seconds(const gchar *duration) {
  gchar **tempo = g_strsplit(duration, ":", 3);

  gint res = 0;
  gint horas = 0, min = 0, sec = 0;

  if (tempo[0] != NULL) horas = g_ascii_strtoll(tempo[0], NULL, 10);
  if (tempo[1] != NULL) min = g_ascii_strtoll(tempo[1], NULL, 10);
  if (tempo[2] != NULL) sec = g_ascii_strtoll(tempo[2], NULL, 10);

  // Calculate total duration in seconds
  res = horas * 3600 + min * 60 + sec;

  g_strfreev(tempo);

  return res;
}

// Função para converter segundos em duração
gchar *convert_seconds_to_duration(gint seconds) {
  gint hours = seconds / 3600;
  gint minutes = (seconds % 3600) / 60;
  gint remaining_seconds = seconds % 60;
  return g_strdup_printf("%02d:%02d:%02d", hours, minutes, remaining_seconds);
}

// Função para validar o formato da duração
gboolean validate_duration_format(gchar *duration) {
  gint len = strlen(duration);
  if (len != 8) {
    return FALSE;
  }

  for (int i = 0; i < len; i++) {
    if (i == 2 || i == 5) {
      if (duration[i] != ':') {
        return FALSE;
      }
    } else {
      if (!isdigit(duration[i])) {
        return FALSE;
      }
    }
  }

  gint hours = g_ascii_strtoll(duration, NULL, 10);
  gint minutes = g_ascii_strtoll(duration + 3, NULL, 10);
  gint seconds = g_ascii_strtoll(duration + 6, NULL, 10);

  if (hours < 0 || hours > 99 || minutes < 0 || minutes > 59 || seconds < 0 ||
      seconds > 59) {
    return FALSE;
  }

  return TRUE;
}

// O campo album_id de uma música, deverá corresponder a um álbum existente e
// válido verificando se existe na hashtable de álbuns
gboolean validate_album_id_in_hashtable(GHashTable *albuns_table, gint id) {
  return g_hash_table_contains(albuns_table, &id);
}

// Função para validar o ano
static gboolean validate_year(const gint year) {
  return (year > 0 && year < 2025);
}

// Função para criar uma estrutura Musica a partir de uma linha de informação
Musica *parse_Musica(gchar *information, GHashTable *artistas_table,
                            GHashTable *albuns_table) {
  if (artistas_table == NULL || albuns_table == NULL) {
    return NULL;
  }

  // Dividir informações
  gchar *clean_information = remove_chars(information, "\"");
  gchar **tokens = g_strsplit(clean_information, ";", 8);
  g_free(clean_information);

  if (g_strv_length(tokens) < 8) {
    g_strfreev(tokens);
    return NULL;
  }

  // Alocar estrutura Musica
  Musica *m = newMusica();
  if (m == NULL) {
    g_strfreev(tokens);
    return NULL;
  }

  // Atribuir valores à estrutura Musica
  gint id = g_ascii_strtoll(tokens[0] + 1, NULL, 10);
  setId(m, id);

  setTitle(m, tokens[1]);

  // Validar e atribuir duração
  if (!validate_duration_format(tokens[4])) {
    g_strfreev(tokens);
    destroyMusica(m);
    return NULL;
  }
  setDuration(m, tokens[4]);

  // Validar e atribuir ID do álbum
  gint id_album = g_ascii_strtoll(tokens[3] + 2, NULL, 10);
  if (!validate_album_id_in_hashtable(albuns_table, id_album)) {
    g_strfreev(tokens);
    destroyMusica(m);
    return NULL;
  }

  setAlbumID_m(m, id_album);

  // Validar e atribuir ID do artista
  gchar *clean_artist_id = remove_chars(tokens[2], "[]\' ");
  gchar **ids = g_strsplit(clean_artist_id, ",", -1);
  gint length = g_strv_length(ids);
  int i;
  gint *ids_artists = g_new(gint, length);  
  for (i = 0; ids[i]; i++) {
    ids_artists[i] = g_ascii_strtoll(ids[i] + 1, NULL, 10);
    Artista *a =
        (Artista *)g_hash_table_lookup(artistas_table, &ids_artists[i]);
    if (!a) {
      g_strfreev(ids);
      g_free(ids_artists);
      return NULL;
    }
    gint aumento = getDuration(m);
    aumentaDiscografia(a, aumento);
  }

  setArtistId_m(m, ids_artists, i);
  setNumeroArtistas(m, i);
  g_free(clean_artist_id);
  g_strfreev(ids);
  g_free(ids_artists);

  // Atribuir gênero
  setGenre(m, tokens[5]);

  // Validar e atribuir ano
  gint year = g_ascii_strtoll(tokens[6], NULL, 10);
  if (!validate_year(year)) {
    g_strfreev(tokens);
    destroyMusica(m);
    return NULL;
  }

  // Limpar tokens e retornar a estrutura
  g_strfreev(tokens);
  return m;
}
