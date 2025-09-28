/**
 * @file Queries.h
 * @brief Header file containing function declarations for processing queries.
 */

#ifndef QUERIES_H
#define QUERIES_H

#include <glib.h>
#include "../include/gestores/gestorArtista.h"
#include "../include/my_recomendador.h"

/**
 * @brief Processes queries from a file.
 * 
 * @param input_filename The name of the input file containing queries.
 * @param utilizadores_filename The name of the file containing user data.
 * @param musica_filename The name of the file containing music data.
 * @param artistas_filename The name of the file containing artist data.
 * @param albuns_filename The name of the file containing album data.
 * @param historico_filename The name of the file containing historical data.
 */
void process_queries_from_file(const gchar *input_filename,
                               const gchar *utilizadores_filename,
                               const gchar *musica_filename,
                               const gchar *artistas_filename,
                               const gchar *albuns_filename,
                               const gchar *historico_filename);

/**
 * @brief Executes query 1.
 * 
 * @param utilizador_table Hash table containing user data.
 * @param artistas_table Hash table containing artist data.
 * @param musica_table Hash table containing music data.
 * @param user_id The ID of the user.
 * @param has_S Flag indicating if the query has a separator.
 * @return gchar* Result of the query.
 */
gchar *Querie1(GHashTable *utilizador_table, GHashTable *artistas_table, 
               GHashTable *musica_table, gchar *user_id, gint has_S);

/**
 * @brief Executes query 2.
 * 
 * @param discografiaArt Array of artist discographies.
 * @param artistas Hash table containing artist data.
 * @param N Number of results to return.
 * @param country The country filter for the query.
 * @param tem_separador Flag indicating if the query has a separator.
 * @return gchar** Array of results.
 */
gchar **Querie2(struct DiscoArtist **discografiaArt, GHashTable *artistas, 
                const gint N, gchar *country, gint tem_separador);

/**
 * @brief Executes query 3.
 * 
 * @param gL Array of genre likes.
 * @param min_age Minimum age filter.
 * @param max_age Maximum age filter.
 * @param has_S Flag indicating if the query has a separator.
 * @return gchar** Array of results.
 */
gchar **Querie3(struct GenreLikes ***gL, gint min_age, gint max_age, 
                gint has_S);

/**
 * @brief Executes query 4.
 * 
 * @param data_inicial Initial date filter.
 * @param data_final Final date filter.
 * @param cumulativeRanking Hash table containing cumulative ranking data.
 * @param artistas_table Hash table containing artist data.
 * @param tem_separador Flag indicating if the query has a separator.
 * @return gchar* Result of the query.
 */
gchar *Querie4(const gchar *data_inicial, const gchar *data_final,
               GHashTable **cumulativeRanking, GHashTable *artistas_table, 
               gint tem_separador);

/**
 * @brief Executes query 5 using my recommendation algorithm.
 * 
 * @param idUtilizadorAlvo Target user ID.
 * @param nr_utilizadores Number of users.
 * @param matriz Matrix containing user data.
 * @param utilizadores_table Hash table containing user data.
 * @param idsUtilizadores Array of user IDs.
 * @return gchar** Array of results.
 */
gchar **Querie5_myrecomenda(gint idUtilizadorAlvo, int nr_utilizadores,
                            gint **matriz, GHashTable *utilizadores_table,
                            gint *idsUtilizadores);

/**
 * @brief Executes query 6.
 * 
 * @param historicos_por_user_table Hash table containing user history data.
 * @param musicas_table Hash table containing music data.
 * @param artistas_table Hash table containing artist data.
 * @param albuns_table Hash table containing album data.
 * @param user_id The ID of the user.
 * @param year The year filter for the query.
 * @param N Number of results to return.
 * @param output Pointer to the output array.
 * @param output_size Pointer to the size of the output array.
 * @param has_S Flag indicating if the query has a separator.
 */
void Querie6(GHashTable *historicos_por_user_table, GHashTable *musicas_table,
             GHashTable *artistas_table, GHashTable *albuns_table,
             gint *user_id, const gchar *year, gint N, gchar ***output,
             gint *output_size, gboolean has_S);

#endif // QUERIES_H
