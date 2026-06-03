/**
 * @file parser_artista.h
 * @brief File that contains functions realated to parsing an artist.
 */
#ifndef PARSER_ARTISTA_H
#define PARSER_ARTISTA_H

#include <glib.h>
#include "entities/Artista.h"

/**
 * @brief Validates the type of an artist.
 * 
 * @param type The type of the artist to validate.
 * @param idConstituent The ID of the constituent to validate.
 * @return 1 if the artist type is valid, 0 otherwise.
 * @note The artist type must be "individual" or "group".
 */
int validArtistType(const gchar *type, const gchar *idConstituent);

/**
 * @brief Creates an Artista struct from a string of information.
 * 
 * @param information A string containing the information to parse.
 * @return A pointer to the newly created Artista struct.
 */
Artista *process_Artista(gchar *information);

#endif // PARSER_ARTISTA_H
