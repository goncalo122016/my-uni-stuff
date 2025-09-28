/**
 * @file executa_query.h
 * @brief Functions for executing queries.
 */

#ifndef EXECUTA_QUERY_H
#define EXECUTA_QUERY_H

#include <ncurses.h>
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <stdlib.h>
#include <glib.h>

#include "../include/interativo/queries.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/gestores/gestorArtista.h"
#include "../include/gestores/gestor_albums.h"
#include "../include/gestores/gestor_historico.h"

/**
 * @brief Executes a query based on the query number.
 * 
 * @param query_num The query number to execute.
 * @param utilizador_table A hash table containing user data.
 * @param musica_table A hash table containing music data.
 * @param artistas_table A hash table containing artist data.
 * @param albums_table A hash table containing album data.
 * @param user_music_table A hash table containing user-music relationship data.
 * @param discografyArtists Array of DiscoArtist structures.
 * @param matriz 2D array of integers.
 * @param idsUtilizadores Array of user IDs.
 * @param nomesGenerosPtr Array of genre names.
 * @param gL 3D array of GenreLikes structures.
 * @param cumulativeRanking Array of GHashTables for cumulative ranking.
 * @param historicos_por_user_table A hash table containing user history data.
 * @note This function is responsible for executing a query based on the query number (1-6).
 */
void executar_query(int query_num, GHashTable *utilizador_table, GHashTable *musica_table, GHashTable *artistas_table, GHashTable *albums_table, GHashTable *user_music_table,DiscoArtist **discografyArtists, gint **matriz, gint *idsUtilizadores, char **nomesGenerosPtr, GenreLikes ***gL, GHashTable **cumulativeRanking, GHashTable *historicos_por_user_table);

#endif