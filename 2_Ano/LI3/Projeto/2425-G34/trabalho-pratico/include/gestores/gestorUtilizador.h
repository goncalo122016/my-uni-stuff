/**
 * @file gestorUtilizador.h
 * @brief Header file for managing user-related operations.
 * 
 * This file contains the declarations of functions and structures used to 
 * manage users and their interactions.
 */

#ifndef GESTOR_UTILIZADOR_H
#define GESTOR_UTILIZADOR_H

#include <glib.h>
#include "../entities/Utilizador.h"
#include "../gestores/gestor_musica.h"

/**
 * @brief Builds a hash table of users from a file.
 *
 * @param filename The name of the .csv file containing user data.
 * @param musictable A hash table containing music data.
 * @param gL A pointer to a list of GenreLikes structures to store user preferences.
 * @return A hash table containing the user structures.
 */
GHashTable *build_utilizador_hash_table(const gchar *filename, GHashTable *musictable, GenreLikes ***gL);

/**
 * @brief Retrieves all users within a specified age range.
 *
 * @param utilizador_table The hash table containing user data.
 * @param minAge The minimum age of users to retrieve.
 * @param maxAge The maximum age of users to retrieve.
 * @return A list of users within the specified age range.
 */
GList* getAllUtilizadores(GHashTable *utilizador_table, int minAge, int maxAge);

/**
 * @brief Retrieves a user by their ID.
 *
 * @param table The hash table containing user data.
 * @param id The ID of the user to retrieve.
 * @return A pointer to the `Utilizador` structure corresponding to the given ID, or NULL if not found.
 */
Utilizador* getUtilizadorById(GHashTable *table, gint id);

/**
 * @brief Retrieves the list of liked music IDs by a user's ID.
 *
 * @param table The hash table containing user data.
 * @param id The ID of the user whose liked music is to be retrieved.
 * @return A GList containing the music IDs liked by the user.
 */
GList* get_liked_musics_by_id(GHashTable *table, gint id);

#endif
