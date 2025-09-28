#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/parsers/parser_artista.h"
#include "../include/parsers/parser_musica.h"
#include "../include/parsers/parser_utilizador.h"
#include "../include/entities/Historico.h"
#include "../include/gestores/gestor_historico.h"
#include "../include/utils.h"

Historico *process_Historico(gchar *information, GHashTable *users_table,
                                    GHashTable *musicas_table,
                                    GHashTable *historicos_por_user_table,
                                    GHashTable *artistas_table) {
  gchar *clean_information = remove_chars(information, "\"");
  gchar **tokens = g_strsplit(clean_information, ";", 6);
  g_free(clean_information);

  if (g_strv_length(tokens) != 6) {
    g_strfreev(tokens);
    return NULL;
  }

  Historico *h = newHistorico();
  gint id = g_ascii_strtoll(tokens[0] + 1, NULL, 10);
  setHistoricoId(h, id);

  gint id_user = g_ascii_strtoll(tokens[1] + 1, NULL, 10);
  if (!validate_key_in_hashtable(users_table, id_user)) {
    g_strfreev(tokens);
    deleteHistorico(h);
    return NULL;
  }

  setHistoricoUser_Id(h, id_user);

  gint id_musica = g_ascii_strtoll(tokens[2] + 1, NULL, 10);
  if (!validate_key_in_hashtable(musicas_table, id_musica)) {
    g_strfreev(tokens);
    deleteHistorico(h);
    return NULL;
  }
  setHistoricoMusica_Id(h, id_musica);

  gchar *date_time = g_strdup(tokens[3]);

  // Usar strtok para separar a data da hora
  gchar *date = strtok(date_time, " ");
  gchar *time = strtok(NULL, " ");

  if (!validate_duration_format(tokens[4])) {
    g_strfreev(tokens);
    deleteHistorico(h);
    g_free(date_time);
    return NULL;
  }
  setHistoricoDuracao(h, tokens[4]);

  Musica *m =
      (Musica *)g_hash_table_lookup(musicas_table, &id_musica);
  incrementReproducoes(m);
  gint *ids_artistas = getArtistId_m(m);
  for (int i = 0; i < getNumeroArtistas(m); i++) {
    Artista *a =
        (Artista *)g_hash_table_lookup(artistas_table, &(ids_artistas[i]));
    gint week = days_between_dates(date) / 7;
    gint aumento = Historico_getDuracao(h);
    aumentaDiscoSemanal(a, aumento, week);
  }
  g_free(ids_artistas);

  gint *id_user_historico = malloc(sizeof(gint));
  *id_user_historico = id_user;
  GPtrArray *user_historicos =
      g_hash_table_lookup(historicos_por_user_table, id_user_historico);

  if (!user_historicos) {
    user_historicos = g_ptr_array_new();
    g_hash_table_insert(historicos_por_user_table, id_user_historico,
                        user_historicos);
  }
  else g_free(id_user_historico);
  g_ptr_array_add(user_historicos, h);

  setHistoricoData(h, date);
  setHistoricoHora(h, time);

  g_free(date_time);

  g_strfreev(tokens);
  return h;
}
