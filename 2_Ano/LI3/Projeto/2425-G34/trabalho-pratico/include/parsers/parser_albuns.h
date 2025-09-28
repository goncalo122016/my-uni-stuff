/**
 * @file
 * @brief Functions for parsing album data.
 */
#ifndef PARSER_ALBUNS_H
#define PARSER_ALBUNS_H

#include <glib.h>
#include "entities/Albums.h"

/**
 * @brief Validates an artist ID.
 * 
 * @param id The artist ID to validate.
 * @param artistas_table A hash table containing artist data.
 * @return TRUE if the artist ID is valid, FALSE otherwise.
 */
gboolean validate_artist_id(const gchar *id, GHashTable *artistas_table);

/**
 * @brief Creates an Album struct from a string of information.
 * 
 * @param information A string containing the information to parse.
 * @param artistas_table A hash table containing artist data.
 * @return A pointer to the newly created Album struct.
 */
Album *process_Albums(gchar *information, GHashTable *artistas_table);

#endif // PARSER_ALBUNS_H
