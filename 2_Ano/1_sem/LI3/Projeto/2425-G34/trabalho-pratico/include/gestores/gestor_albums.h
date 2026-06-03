/**
 * @file gestor_album.h
 * @brief Header file for album management functions.
 * 
 * This file contains the declarations of functions used to build a hash table
 * of albums from a file, calculate the most listened album, and update album statistics.
 */

#ifndef GESTOR_ALBUMS_H
#define GESTOR_ALBUMS_H

#include <glib.h>
#include "../entities/Musica.h"

/**
 * @brief Builds a hash table of albums from a CSV file.
 *
 * @param filename The name of the CSV file containing album data.
 * @param artistas_table A hash table containing artist data.
 * @return A hash table containing the album structures.
 */
GHashTable *build_albums_hash(const gchar *filename, GHashTable *artistas_table);

/**
 * @brief Calculates the most listened album from album statistics.
 *
 * @param album_stats A hash table containing the statistics of each album.
 * @param albuns_table A hash table containing the album data.
 * @return A string representing the ID of the most listened album.
 */
gint calcularAlbumMaisOuvido(GHashTable *album_stats, GHashTable *albuns_table);


/**
 * @brief Updates album statistics with a new play.
 *
 * @param musica A pointer to the `Musica` structure containing the music information.
 * @param album_stats A hash table containing the statistics for albums.
 * @param duracao The duration of the play (in seconds) to be added to the album statistics.
 */
void atualizarEstatisticasAlbum(Musica *musica, GHashTable *album_stats, gint duracao);

#endif // GESTOR_ALBUMS_H
