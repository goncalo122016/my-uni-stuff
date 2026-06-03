/**
 * @file parser_utilizador.h
 * @brief File containing the functions to parse Utilizador structs.
 */
#ifndef PARSER_UTILIZADOR_H
#define PARSER_UTILIZADOR_H

#include <glib.h>
#include "entities/Utilizador.h"
#include "gestores/gestor_musica.h"

/**
 * @brief Creates a Utilizador struct from a string of information.
 * 
 * @param information A string containing the information to parse.
 * @param musictable A hash table containing music data.
 * @return A pointer to the newly created Utilizador struct.
 */
Utilizador *parse_Utilizador(gchar *information, GHashTable *musictable, GenreLikes ***gL);

#endif // PARSER_UTILIZADOR_H
