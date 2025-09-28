/**
 * @file tela_queries.h
 * @brief Header file for the tela_queries function and related includes.
 *
 * This file contains the declaration of the tela_queries function and includes
 * necessary libraries and other header files required for its implementation.
 */

#ifndef TELA_QUERIES_H
#define TELA_QUERIES_H

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
#include "../include/my_recomendador.h"
#include "../include/Queries.h"
#include "../include/Output.h"
#include "../include/utils.h"

/**
 * @brief Handles queries in interactive mode.
 * 
 * @param utilizador_table GHashTable containing user data.
 * @param musica_table GHashTable containing music data.
 * @param artistas_table GHashTable containing artist data.
 * @param albums_table GHashTable containing album data.
 * @param user_music_table GHashTable containing user-music relationship data.
 * @param discografyArtists Array of DiscoArtist structures.
 * @param matriz 2D array of integers.
 * @param idsUtilizadores Array of user IDs.
 * @param nomesGenerosPtr Array of genre names.
 * @param gL 3D array of GenreLikes structures.
 * @param cumulativeRanking Array of GHashTables for cumulative ranking.
 * @param historicos_por_user_table GHashTable containing user history data.
 * @note This function is responsible for handling queries in the interactive mode of the application.
 */
void tela_queries(GHashTable *utilizador_table, GHashTable *musica_table, GHashTable *artistas_table, GHashTable *albums_table, GHashTable *user_music_table, DiscoArtist **discografyArtists, gint **matriz, gint *idsUtilizadores, char **nomesGenerosPtr,GenreLikes ***gL, GHashTable **cumulativeRanking, GHashTable *historicos_por_user_table);

#endif // TELA_QUERIES_H