/**
 * @file gestor_historico.h
 * @brief Header file for managing `Historico` data.
 * 
 * This file contains the declarations of functions used to manipulate
 * `Historico` data, including converting strings to lowercase, building
 * a hash table from a CSV file, and various operations on user and music history.
 */
#ifndef GESTOR_HISTORICO_H
#define GESTOR_HISTORICO_H

#include <glib.h>
#include <time.h>

#define INICIAL_DATE 1527379200

/**
 * @struct user_musicas
 * @brief Structure to store user music data.
 */
typedef struct user_musicas user_musicas;

/**
 * @brief Builds a hash table of `Historico` data from a CSV file.
 * 
 * @param filename The path to the CSV file.
 * @param users_table A hash table containing user data.
 * @param musicas_table A hash table containing music data.
 * @param user_music_table A hash table that maps users to the music they have listened to.
 * @param historicos_por_user_table A hash table that maps users to their `Historico` data.
 * @param artistas_table A hash table containing artist data.
 * @return A pointer to the newly created hash table containing `Historico` data.
 */
GHashTable *build_historico_hash_table(const gchar *filename, GHashTable *users_table, GHashTable *musicas_table, GHashTable *user_music_table, GHashTable *historicos_por_user_table, GHashTable *artistas_table);

/**
 * @brief Gets the `Historico` data of a user by ID.
 * 
 * @param `Historico` The hash table containing `Historico` data.
 * @param user_id The ID of the user to search for.
 * @return A list of music IDs that the user has listened to.
 */
GList* get_historico_musicas_by_id(GHashTable *Historico, const gchar *user_id);

/**
 * @brief Parses a date string into a struct tm.
 * 
 * @param date_str The date string in the format "YYYY-MM-DD".
 * @return A struct tm representing the parsed date.
 */
struct tm parse_date(const char* date_str);

/**
 * @brief Calculates the number of days between two dates.
 * 
 * @param date1_str The first date string in the format "YYYY-MM-DD".
 * @return The number of days between the two dates.
 */
int days_between_dates(const char* date1_str);

/**
 * @brief Retrieves the list of music IDs listened to by a user.
 * 
 * @param um Pointer to the `user_musicas` structure.
 * @return A list of music IDs that the user has listened to.
 */
GList *getUserMusicas(user_musicas *um);

/**
 * @brief Frees the memory allocated for a `user_musicas` structure.
 * 
 * @param data Pointer to the `user_musicas` structure to be freed.
 */
void destroy_user_musicas(gpointer data);

/**
 * @brief Populates a hash table that maps each user ID to a list of associated `Historico` entries.
 * 
 * @param historico_table The hash table containing all `Historico` data.
 * @param historicos_por_user_table The hash table to be populated with user IDs mapped to their histories.
 */
void populateHistoricosPorUser(GHashTable *historico_table, GHashTable *historicos_por_user_table);

/**
 * @brief Calculates the most listened day from and user's `Historico` data.
 * 
 * @param day_stats A hash table containing the statistics for each day.
 * @return A string representing the day with the most listens.
 */
gchar *calcularDiaMaisOuvido(GHashTable *day_stats);

/**
 * @brief Calculates the most listened hour from an user's `Historico` data.
 * 
 * @param freq_hours An array of integers representing the frequency of listens for each hour.
 * @return A string representing the hour with the most listens.
 */
gchar *calcularHoraMaisOuvida(gint freq_hours[24]);

#endif // GESTOR_HISTORICO_H
