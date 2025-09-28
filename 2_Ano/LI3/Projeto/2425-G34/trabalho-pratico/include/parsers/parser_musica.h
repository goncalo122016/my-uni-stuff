/**
 * @file parser_musica.h
 * @brief Header file for the Musica parser.
 */
#ifndef PARSER_MUSICA_H
#define PARSER_MUSICA_H

#include <glib.h>
#include "entities/Musica.h"

/**
 * @brief Converts seconds to a duration string in the format hh:mm:ss.
 * 
 * @param seconds The number of seconds to convert.
 * @return A string representing the duration in hh:mm:ss format.
 */
gchar* convert_seconds_to_duration(gint seconds);

/**
 * @brief Validates the format of a duration string.
 * 
 * @param duration The duration string to validate.
 * @return TRUE if the duration format is valid, FALSE otherwise.
 * @note The duration format is hh:mm:ss.
 */
gboolean validate_duration_format(gchar *duration);

/**
 * @brief Checks if a music ID exists in the hash table.
 * 
 * @param hashtable The hash table to check.
 * @param id The music ID to validate.
 * @return TRUE if the music ID exists, FALSE otherwise.
 */
gboolean validate_music_id_in_hashtable(GHashTable *hashtable, const gchar *id);

/**
 * @brief Creates a Musica struct from a string of information.
 * 
 * @param information A string containing the information to parse.
 * @param artistas_table A hash table containing artist data.
 * @param albuns_table A hash table containing album data.
 * @return A pointer to the newly created Musica struct.
 */
Musica *parse_Musica(gchar *information, GHashTable *artistas_table, GHashTable *albuns_table);

#endif // PARSER_MUSICA_H
