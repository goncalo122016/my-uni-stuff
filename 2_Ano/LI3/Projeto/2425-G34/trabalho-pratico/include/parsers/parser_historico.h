/**
 * @file parser_historico.h
 * @brief File containing the functions to parse Historico data.
 */
#ifndef PARSER_HISTORICO_H
#define PARSER_HISTORICO_H

#include <glib.h>
#include "entities/Historico.h"

/**
 * @brief Creates a Historico struct from a string of information.
 * 
 * @param information A string containing the information to parse.
 * @param users_table A hash table containing user data.
 * @param musicas_table A hash table containing music data.
 * @param historicos_por_user_table A hash table containing Historico data per user.
 * @param artistas_table A hash table containing artist data.
 * @return A pointer to the newly created Historico struct.
 */
Historico *process_Historico(gchar *information, GHashTable *users_table, GHashTable *musicas_table, GHashTable *historicos_por_user_table, GHashTable *artistas_table);

#endif // PARSER_HISTORICO_H
