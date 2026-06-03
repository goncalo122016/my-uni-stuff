#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/entities/Utilizador.h"
#include "../../include/parsers/parser_utilizador.h"
#include "../../include/Output.h"
#include "../../include/utils.h"

#define MAX_SIZE 2048

// Função para construir uma Hash Table de utilizadores a partir de um ficheiro
// .CSV
GHashTable *build_utilizador_hash_table(const gchar *filename, GHashTable *musictable, GenreLikes ***gL) {
  static gboolean header_written = FALSE;  // Variável estática para controlar se o cabeçalho já foi escrito
  
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    printf("Erro: Não foi possível abrir o ficheiro %s.\n", filename);
    return NULL;
  }

  // Ignorar a primeira linha (cabeçalho) do ficheiro
  gchar header_line[MAX_SIZE];
  if (fgets(header_line, sizeof(header_line), file) == NULL) {
    printf(
        "Erro: O ficheiro está vazio ou ocorreu um problema ao ler o "
        "cabeçalho.\n");
    fclose(file);
    return NULL;
  }

  GHashTable *utilizador_table = g_hash_table_new_full(
      g_int_hash, g_int_equal, g_free, (GDestroyNotify)deleteUtilizador);

  gchar line[MAX_SIZE];
  while (fgets(line, sizeof(line), file) != NULL) {
    line[strcspn(line, "\n")] = 0;

    // Parse da linha para criar o utilizador
    Utilizador *utilizador = parse_Utilizador(line, musictable, gL);
    if (utilizador != NULL) {
      // Insere o utilizador na tabela hash usando o Username como chave
      gint *id = g_malloc(sizeof(gint));
      *id = getUsername(utilizador);
      g_hash_table_insert(utilizador_table, id,
                          utilizador);
    } else {
      // Adicionar o cabeçalho ao ficheiro de erros
      if (!header_written) {
        write_output_file("resultados/users_errors.csv", header_line);
        header_written = TRUE;
      }
      line[strcspn(line, "\0")] = '\n';
      write_output_file("resultados/users_errors.csv", line);
    }
  }
  fclose(file);
  return utilizador_table;
}

// Função para obter todos os utilizadores com idade entre minAge e maxAge
GList* getAllUtilizadores(GHashTable *utilizador_table, int minAge, int maxAge) {
    // Verificar se a tabela não é NULL
    if (utilizador_table == NULL) {
        g_warning("Tabela de utilizadores está vazia!");
        return NULL;
    }

    GList *utilizadores = NULL;
    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init(&iter, utilizador_table);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        if (value == NULL) {
            g_warning("Encontrado valor NULL na tabela de utilizadores.");
            continue;
        }

        Utilizador *u = (Utilizador *)value;
        
        // Verificação da idade com mensagens de depuração
        int age = getAge(u);
        if (age < 0) { 
            g_warning("Idade inválida ou Utilizador nulo.");
            continue;
        }

        //printf("Verificando Utilizador: %s, Idade: %d\n", u->Username, age);

        if (age >= minAge && age <= maxAge) {
            utilizadores = g_list_append(utilizadores, u);
        }
    }

    return utilizadores;
}

// Dado um username (que funciona como Id), devolve o utilizador correspondente
Utilizador* getUtilizadorById(GHashTable *table, gint id) {
    Utilizador *u = g_hash_table_lookup(table, &id);
    return (u) ? u : NULL;
}

GList* get_liked_musics_by_id(GHashTable *table, gint id) {
    Utilizador *u = getUtilizadorById(table, id);
    if (u == NULL ) {
        return NULL;
    }

    gint *musicas_gostadas = getLikedMusics(u);
    gint length = getNumeroMusicas(u);
    GList *liked_musics = NULL;
    for (int i = 0; i < length; i++) {
        liked_musics = g_list_append(liked_musics, &musicas_gostadas[i]);
    }

    return liked_musics;
}