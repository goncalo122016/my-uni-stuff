/**
 * @file my_recomendador.h
 * @brief Functions for recommending similar users based on music preferences.
 */
#ifndef RECOMENDADOR_H
#define RECOMENDADOR_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <glib.h>
#include "../include/gestores/gestorUtilizador.h"
#include "../include/entities/Musica.h"

/**
 * @brief Compares two users based on their similarity scores.
 *
 * @param a Pointer to the first user to compare (should be a pointer to a similarity score).
 * @param b Pointer to the second user to compare (should be a pointer to a similarity score).
 * @return An integer less than, equal to, or greater than zero depending on whether 
 *         the first user is considered to be respectively less than, equal to, or greater 
 *         than the second user based on their similarity scores.
 */
int compareUserSimilarity2(const void *a, const void *b);

/**
 * @brief Recommends similar users based on music preferences.
 *
 * @param idUtilizadorAlvo The ID of the target user.
 * @param matrizClassificacaoMusicas A matrix representing the music preferences of all users.
 * @param idsUtilizadores Array containing the IDs of all users.
 * @param numUtilizadores The total number of users.
 * @param numGeneros The number of music genres.
 * @param numRecomendacoes The number of recommended users to return.
 * @return A list of recommended users based on similarity.
 */
char **my_recomendaUtilizadores(gint idUtilizadorAlvo, int **matrizClassificacaoMusicas, 
                                 gint *idsUtilizadores,
                                 int numUtilizadores, int numGeneros, int numRecomendacoes);

/**
 * @brief Frees the memory allocated for a matrix.
 *
 * @param matrix The matrix to be freed.
 * @param n The size of the matrix (number of rows).
 */
void freeMatrix(gint **matrix, gint n);

/**
 * @brief Creates a matrix representing user preferences for music genres.
 *
 * @param table A hash table containing user data.
 * @param numGeneros The number of music genres.
 * @return A matrix representing user preferences.
 */
gint **makeMatrix(GHashTable *table, gint numGeneros);

/**
 * @brief Fills the matrix with user music preferences.
 *
 * @param user_music_table A hash table containing the relationship between users and their liked music.
 * @param musicas A hash table containing music data.
 * @param utilizadores_table A hash table containing user data.
 * @param matrix The matrix to be filled with user music preferences.
 * @param idsUtilizadores Array of user IDs.
 * @param numUtilizadores The number of users.
 * @param nomesGeneros Array of genre names.
 * @param numGeneros The number of music genres.
 */
void fillMatrixWithUserLikes(GHashTable *user_music_table, GHashTable *musicas, GHashTable *utilizadores_table,
                             gint **matrix, gint *idsUtilizadores, 
                             int numUtilizadores, char **nomesGeneros, int numGeneros);

#endif // RECOMENDADOR_H
