/**
 * @file gestor_musica.h
 * @brief Header file for music management functions and structures.
 * 
 * This file contains the declarations of functions and structures used for
 * processing music data, managing user likes, and handling genre likes.
 */

#ifndef GESTOR_MUSICA_H
#define GESTOR_MUSICA_H

#include <glib.h>
#include "../entities/Musica.h"
#include "../entities/Utilizador.h"

#define MIN_AGE 17
#define MAX_AGE 60

/**
 * @struct UserAgeTuple
 * @brief Structure to store user ID and age.
 */
typedef struct UserAgeTuple userAge;

/**
 * @struct GenreLikes
 * @brief Structure to store genre and its corresponding likes.
 */
typedef struct GenreLikes GenreLikes;

/**
 * @brief Processes the input file and creates a hash table of `Musica` structures.
 *
 * @param filename The name of the file containing music data.
 * @param artistas_table A hash table containing artist data.
 * @param albuns_table A hash table containing album data.
 * @return A hash table containing `Musica` structures.
 */
GHashTable *build_musica_hash_table(const gchar *filename, GHashTable *artistas_table, GHashTable *albuns_table);

/**
 * @brief Creates an array of GenreLikes pointers.
 * 
 * @return A pointer to the newly created 2D array of GenreLikes pointers.
 */
GenreLikes ***createGenreLikesTotal();

/**
 * @brief Creates a new GenreLikes structure.
 * 
 * @return A pointer to the newly created GenreLikes structure.
 */
GenreLikes *createGenreLikes();

/**
 * @brief Retrieves the number of likes for a GenreLikes structure.
 * 
 * @param gL Pointer to the GenreLikes structure.
 * @return The number of likes.
 */
gint getLikes_gL(GenreLikes *gL);

/**
 * @brief Increases the number of likes for a GenreLikes structure.
 * 
 * @param gL Pointer to the GenreLikes structure.
 * @param aumento The amount to increase the likes by.
 */
void aumentaLikes(GenreLikes *gL, gint aumento);

/**
 * @brief Frees the memory used by a 2D array of GenreLikes structures.
 * 
 * @param gL Pointer to the 2D array of GenreLikes pointers to be freed.
 */
void freeGenreLikesTotal(GenreLikes ***gL);

/**
 * @brief Sets the genre field of a GenreLikes structure.
 * 
 * @param gL Pointer to the GenreLikes structure.
 * @param genre The genre string to set.
 */
void setGenre_gL(GenreLikes *gL, gchar *genre);

/**
 * @brief Compares the popularity of two genres.
 * 
 * @param a Pointer to the first GenreLikes structure.
 * @param b Pointer to the second GenreLikes structure.
 * @return The result of the comparison.
 */
int compareGenrePopularity(const void *a, const void *b);

/**
 * @brief Retrieves the genre from a GenreLikes structure.
 * 
 * @param gL Pointer to the GenreLikes structure.
 * @return The genre string.
 */
gchar *getGenre_gL(GenreLikes *gL);

/**
 * @brief Frees the memory of a GenreLikes structure.
 * 
 * @param gL Pointer to the GenreLikes structure to be freed.
 */
void destroyGenreLike(GenreLikes *gL);

/**
 * @brief Calculates the most listened genre based on genre statistics.
 * 
 * @param genre_stats A hash table containing genre statistics.
 * @return The genre with the most listens.
 */
gchar *calcularGeneroMaisOuvido(GHashTable *genre_stats);

/**
 * @brief Updates genre statistics based on a music play.
 * 
 * @param musica Pointer to the `Musica` structure representing the music played.
 * @param genre_stats Hash table containing genre statistics.
 * @param duracao The duration of the music played.
 */
void atualizarEstatisticasGenero(Musica *musica, GHashTable *genre_stats, gint duracao);

/**
 * @brief Gets the genre of a music by its ID.
 * 
 * @param musicas Hash table containing `Musica` structures.
 * @param id The ID of the music.
 * @return The genre of the music.
 */
gchar *get_genre_by_id(GHashTable *musicas, gint *id);

#endif // GESTOR_MUSICA_H