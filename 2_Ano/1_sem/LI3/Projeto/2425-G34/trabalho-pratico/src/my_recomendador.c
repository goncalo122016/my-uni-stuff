#include <glib.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../include/entities/Historico.h"
#include "../include/entities/Musica.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/gestores/gestor_historico.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/utils.h"

typedef struct {
  int index;
  int similarity;
  gint id;
} UserSimilarity;

// Função de comparação para qsort
int compareUserSimilarity2(const void *a, const void *b) {
  const UserSimilarity *userA = (const UserSimilarity *)a;
  const UserSimilarity *userB = (const UserSimilarity *)b;

  if (userA->id == 0 || userB->id == 0) {
    return 0;
  }

  // Compara pela  (ordem crescente)
  if (userA->similarity != userB->similarity) {
    return userA->similarity - userB->similarity;
  }

  return userA->id > userB->id;
}

// Função para recomendar utilizadores semelhantes
char **my_recomendaUtilizadores(gint idUtilizadorAlvo,
                                int **matrizClassificacaoMusicas,
                                gint *idsUtilizadores, int numUtilizadores,
                                int numGeneros, int numRecomendacoes) {
  if (numRecomendacoes == 0) return NULL;
  int targetIndex = -1;

  // Encontra o índice do utilizador alvo
  for (int i = 0; i < numUtilizadores; i++) {
    if (idsUtilizadores[i] == idUtilizadorAlvo) {
      targetIndex = i;
      break;
    }
  }

  if (targetIndex == -1) {
    return NULL;
  }

  // Aloca um array para armazenar os dados de similaridade
  UserSimilarity *userSimilarities =
      malloc((numUtilizadores) * sizeof(UserSimilarity));
  if (userSimilarities == NULL) {
    perror("Erro ao alocar memória para userSimilarities");
    return NULL;
  }
  int similarityCount = 0;

  for (int i = 0; i < numUtilizadores; i++) {
    if (i == targetIndex) continue;

    int similarity = 0;
    for (int g = 0; g < numGeneros; g++) {
      similarity += abs(matrizClassificacaoMusicas[targetIndex][g] -
                        matrizClassificacaoMusicas[i][g]);
    }

    userSimilarities[similarityCount].index = i;
    userSimilarities[similarityCount].similarity = similarity;
    userSimilarities[similarityCount].id = idsUtilizadores[i];
    similarityCount++;
  }

  qsort(userSimilarities, similarityCount, sizeof(UserSimilarity),
        compareUserSimilarity2);

  // Prepara o array de recomendações
  char **recommendations = malloc(numRecomendacoes * sizeof(char *));
  if (recommendations == NULL) {
    return NULL;
  }
  for (int i = 0; i < numRecomendacoes && i < similarityCount; i++) {
    gchar *id = malloc(sizeof(gchar) * 9);
    gchar *zeros =
        createZerosString( 7 - contar_digitos(userSimilarities[i].id));
    snprintf(id, 9 , "U%s%d", zeros, userSimilarities[i].id);
    recommendations[i] = id;
    g_free(zeros);
  }

  // Libera a memória temporária
  free(userSimilarities);

  return recommendations;
}

// free matriz
void freeMatrix(gint **matrix, gint n) {
  for (gint i = 0; i < n; i++) {
    g_free(matrix[i]);
  }
  g_free(matrix);
}

// Funcao que faz uma matriz com n users e 10 generos musicais

gint **makeMatrix(GHashTable *table, gint numGeneros) {
  gint n = g_hash_table_size(table);
  gint **matrix = g_malloc(n * sizeof(gint *));
  for (gint i = 0; i < n; i++) {
    matrix[i] = g_malloc(numGeneros * sizeof(gint));
    memset(matrix[i], 0, numGeneros * sizeof(gint));
  }
  return matrix;
}

// Funcao que preenche a matriz com os likes dos utilizadores sendo que recebe a
// tabela de utilizadores, a matriz ,os ids dos utilizadores , o numero de
// utilizadores, os nomes dos generos e o numero de generos
void fillMatrixWithUserLikes(GHashTable *user_music_table, GHashTable *musicas,
                             GHashTable *utilizadores_table, gint **matrix,
                             gint *idsUtilizadores, int numUtilizadores,
                             char **nomesGeneros, int numGeneros) {
  GHashTableIter iter;
  gpointer key, value;
  int i = 0;

  if (user_music_table == NULL || musicas == NULL ||
      utilizadores_table == NULL) {
    fprintf(stderr, "Erro: Tabelas de entrada são nulas.\n");
    return;
  }

  g_hash_table_iter_init(&iter, utilizadores_table);

  // Itera sobre todos os utilizadores na tabela de utilizadores
  while (g_hash_table_iter_next(&iter, &key, &value)) {
    if (i >= numUtilizadores) {
      fprintf(stderr,
              "Aviso: Número de utilizadores excede o tamanho da matriz.\n");
      break;
    }

    gint *user_id = malloc(sizeof(gint));
    *user_id = *(gint *)key;
    user_musicas *um =
        (user_musicas *)g_hash_table_lookup(user_music_table, user_id);

    // Adiciona o ID do utilizador à matriz
    idsUtilizadores[i] = *user_id;
    g_free(user_id);

    if (um == NULL || getUserMusicas(um) == NULL) {
      for (int g = 0; g < numGeneros; g++) {
        matrix[i][g] = 0;  // Sem alteração
      }
      i++;
      continue;
    }

    // Obtém a lista de músicas do utilizador
    GList *music_list = getUserMusicas(um);

    for (GList *l = music_list; l != NULL; l = l->next) {
      gint *idMusica = malloc(sizeof(gint));
      *idMusica = *(gint *)l->data;

      // Obtém o gênero da música pelo ID
      gchar *generoMusica = get_genre_by_id(musicas, idMusica);
      if (generoMusica == NULL) {
        continue;
      }

      // Atualiza a matriz incrementando o contador do gênero correspondente
      for (int g = 0; g < numGeneros; g++) {
        if (strcmp(generoMusica, nomesGeneros[g]) == 0) {
          matrix[i][g]++;
          break;
        }
      }
      g_free(generoMusica);
      g_free(idMusica);
    }

    i++;
  }
}
