#include "../../include/gestores/gestor_historico.h"

#include <ctype.h>
#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "../../include/Output.h"
#include "../../include/parsers/parser_historico.h"
#include "../../include/Queries.h"
#include "../../include/entities/Artista.h"
#include "../../include/entities/Historico.h"
#include "../../include/gestores/gestorArtista.h"
#include "../../include/gestores/gestor_historico.h"
#include "../../include/gestores/gestor_musica.h"
#include "../include/utils.h"

#define MAX_SIZE 1024

struct user_musicas {
  gint user_id;    // ID do utilizador
  GList *musicas;  // Lista de IDs de músicas
};

GList *getUserMusicas(user_musicas *um) { return um->musicas; }

GHashTable *build_historico_hash_table(const gchar *filename,
                                       GHashTable *users_table,
                                       GHashTable *musicas_table,
                                       GHashTable *user_music_table,
                                       GHashTable *historicos_por_user_table,
                                       GHashTable *artistas_table) {
  static gboolean header_written = FALSE;

  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    printf("Erro: Não foi possível abrir o ficheiro %s\n", filename);
    return NULL;
  }

  if (users_table == NULL) {
    printf("Erro: Tabela de utilizadores não está inicializada.\n");
    fclose(file);
    return NULL;
  }

  if (musicas_table == NULL) {
    printf("Erro: Tabela de músicas não está inicializada.\n");
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

  GHashTable *historico_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, free, (GDestroyNotify)deleteHistorico);

  gchar line[512];
  while (fgets(line, sizeof(line), file) != NULL) {
    line[strcspn(line, "\n")] = 0;

    Historico *h =
        process_Historico(line, users_table, musicas_table,
                          historicos_por_user_table, artistas_table);

    if (h != NULL) {
      gint *id = malloc(sizeof(gint));
      *id = Historico_getId(h);
      g_hash_table_insert(historico_table, id, h);

      gint *user_id = malloc(sizeof(gint));
      *user_id = (Historico_getUser_Id(h));
      gint *music_id = malloc(sizeof(gint));
      *music_id = (Historico_getMusica_Id(h));

      user_musicas *um = g_hash_table_lookup(user_music_table, user_id);
      if (um == NULL) {
        um = g_malloc(sizeof(user_musicas));
        um->user_id = *user_id;
        um->musicas = g_list_append(NULL, music_id);
        gint *user_id_key = g_malloc(sizeof(gint));
        *user_id_key = *user_id;
        g_hash_table_insert(user_music_table, user_id_key, um);
      } else {
        um->musicas = g_list_append(um->musicas, music_id);
      }
      g_free(user_id);
    } else {
      if (!header_written) {
        write_output_file("resultados/history_errors.csv", header_line);
        header_written = TRUE;
      }
      line[strcspn(line, "\0")] = '\n';
      write_output_file("resultados/history_errors.csv", line);
    }
  }
  fclose(file);
  return historico_table;
}

void destroy_user_musicas(gpointer data) {
  user_musicas *um = (user_musicas *)data;

  // Free each music_id in the musicas list
  g_list_free_full(um->musicas, g_free);

  // Free the struct itself
  g_free(um);
}

struct tm parse_date(const char *date_str) {
  struct tm date = {0};  // Inicializa a struct tm com zeros
  sscanf(date_str, "%d/%d/%d", &date.tm_year, &date.tm_mon, &date.tm_mday);

  // Ajuste para o formato de struct tm:
  // - Mês vai de 0 a 11 (então subtraímos 1 do mês)
  // - Ano precisa ser ajustado para "anos desde 1900" (subtraímos 1900)
  date.tm_mon -= 1;
  date.tm_year -= 1900;

  return date;
}

// Função para calcular a diferença em dias entre duas datas
int days_between_dates(const char *date1_str) {
  // Converte as strings de data para structs tm
  struct tm date1 = parse_date(date1_str);

  // Converte as structs tm para timestamps em segundos
  time_t timestamp1 = mktime(&date1);

  if (timestamp1 == -1) {
    fprintf(stderr, "Erro ao converter datas.\n");
    return -1;
  }

  // Calcula a diferença em segundos e converte para dias
  double difference = difftime(timestamp1, (time_t)INICIAL_DATE);
  return (int)(difference / (60 * 60 * 24));
}

// Função para criar a HashTable que mapeia cada user_id para uma lista de
// históricos associados
void populateHistoricosPorUser(GHashTable *historico_table,
                               GHashTable *historicos_por_user_table) {
  GHashTableIter iter;
  gpointer key, value;

  g_hash_table_iter_init(&iter, historico_table);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    Historico *historico = (Historico *)value;
    gint user_id = Historico_getUser_Id(historico);

    GPtrArray *user_historicos =
        g_hash_table_lookup(historicos_por_user_table, &user_id);
    if (!user_historicos) {
      user_historicos = g_ptr_array_new();
      g_hash_table_insert(historicos_por_user_table, &user_id, user_historicos);
    }
    g_ptr_array_add(user_historicos, historico);
  }
}

gchar *calcularDiaMaisOuvido(GHashTable *day_stats) {
  GHashTableIter iter;
  gpointer key, value;
  gint max_day_count = -1;
  gchar *most_music_day = NULL;

  g_hash_table_iter_init(&iter, day_stats);
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    gint *day_count = (gint *)value;
    gchar *day = (gchar *)key;

    if (*day_count > max_day_count ||
        (*day_count == max_day_count &&
         compare_dates(day, most_music_day) > 0)) {
      most_music_day = day;
      max_day_count = *day_count;
    }
  }

  if (most_music_day) {
    return g_strdup(most_music_day);
  } else {
    return NULL;
  }
}

gchar *calcularHoraMaisOuvida(gint freq_hours[24]) {
  gint max_hour_count = 0;
  gchar *most_common_hour = NULL;

  for (int i = 0; i < 24; i++) {
    if (freq_hours[i] > max_hour_count) {
      max_hour_count = freq_hours[i];
      g_free(most_common_hour);
      most_common_hour = g_strdup_printf("%02d", i);
    }
  }

  if (most_common_hour) {
    return most_common_hour;
  } else {
    return NULL;
  }
}