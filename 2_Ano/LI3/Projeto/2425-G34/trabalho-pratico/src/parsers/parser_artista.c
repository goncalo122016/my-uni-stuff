#include <ctype.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../include/gestores/gestorArtista.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/Output.h"
#include "../include/utils.h"

// Função que verifica se o artista é válido
int validArtistType(gchar *type, gchar *idConstituent) {
  return (g_ascii_strcasecmp(type, "individual") == 0 &&
          g_strcmp0(idConstituent, "[]") == 0) ||
         (g_ascii_strcasecmp(type, "group") == 0 &&
          g_strcmp0(idConstituent, "[]") != 0);
}

// Função para retirar de uma linha de texto o artista validando os campos
// necessários
Artista *process_Artista(gchar *information) {
  gchar *clean_information = remove_chars(information, "\"");
  gchar **tokens = g_strsplit(clean_information, ";", 7);
  g_free(clean_information);
  int len = strlen(tokens[6]);
  tokens[6][len-1] = '\0';
  if (!validArtistType(tokens[6], tokens[4])) {
    g_strfreev(tokens);
    return NULL;
  }
  Artista *a = newArtist();
  gint id = g_ascii_strtoll(tokens[0] + 1, NULL, 10);
  setArtistId(a, id);
  setArtistName(a,tokens[1]);
  setArtistRecipePerStream(a, g_ascii_strtod(tokens[3], NULL));
  if (g_strcmp0(tokens[4], "[]") == 0) {
    setArtistIdConstituent(a, NULL, 0);  // No constituents
  } else {
    gchar *clean_brackets = remove_chars(tokens[4], "[]\'");
    gchar **constituents = g_strsplit(clean_brackets, ",", -1);
    gint length = g_strv_length(constituents);
    int i;
    gint *ids = g_new(gint, length);
    for (i = 0; i < length; i++){
      ids[i] = g_ascii_strtoll(constituents[i] + 1, NULL, 10);
    }
    setArtistIdConstituent(a, ids, i);
    setNumeroConstituents(a, i);
    g_free(clean_brackets);
    g_strfreev(constituents);
    g_free(ids);
  }
  setArtistCountry(a,tokens[5]);
  gint type = (g_ascii_strcasecmp(tokens[6], "individual") == 0) ? 0 : 1;
  setArtistType(a, type);
  g_strfreev(tokens);
  return a;
}