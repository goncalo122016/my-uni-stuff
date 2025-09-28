/**
 * @file queries.h
 * @brief Definition of functions that enable user interaction.
 */

#ifndef QUERIES_INT_H
#define QUERIES_INT_H

#include <ncurses.h>
#include <string.h>
#include <glib.h>

#include "../include/Queries.h"

/**
 * @brief Function that executes Query 1 interactively.
 * 
 * @param user_table Hash table containing the users.
 * @param artist_table Hash table containing the artists.
 * @param music_table Hash table containing the songs.
 */
void query1_interativo(GHashTable *user_table, GHashTable *artist_table, GHashTable *music_table);

/**
 * @brief Function that executes Query 2 interactively.
 * 
 * @param discography Array of artist discographies.
 * @param artists Hash table containing the artists.
 */
void query2_interativo(DiscoArtist **discography, GHashTable *artists);

/**
 * @brief Function that executes Query 3 interactively.
 * 
 * @param genre_likes Array of genre preferences.
 */
void query3_interativo(GenreLikes ***genre_likes);

/**
 * @brief Function that executes Query 4 interactively.
 * 
 * @param cumulative_ranking Array of hash tables containing cumulative rankings.
 * @param artist_table Hash table containing the artists.
 */
void query4_interativo(GHashTable **cumulative_ranking, GHashTable *artist_table);

/**
 * @brief Function that executes Query 5 interactively.
 * 
 * @param user_music_table Hash table mapping users to songs.
 * @param num_users Total number of users.
 * @param matrix Song classification matrix.
 * @param user_table Hash table containing the users.
 * @param user_ids Array of user IDs.
 * @param genre_names Array containing genre names.
 */
void query5_interativo(GHashTable *user_music_table, int num_users,
                        gint **matrix, GHashTable *user_table,
                        gint *user_ids, char **genre_names);

/**
 * @brief Function that executes Query 6 interactively.
 * 
 * @param user_history_table Hash table containing user histories.
 * @param music_table Hash table containing the songs.
 * @param artist_table Hash table containing the artists.
 * @param album_table Hash table containing the albums.
 */
void query6_interativo(GHashTable *user_history_table, GHashTable *music_table,
                        GHashTable *artist_table, GHashTable *album_table);

#endif // QUERIES_INT_H
