/**
 * @file Musica.h
 * @brief Header file for the `Musica` entity.
 * 
 * This file contains the definition of the `Musica` structure and the 
 * declarations of functions to manipulate `Musica` objects.
 */

#ifndef MUSICA_H
#define MUSICA_H

#include <glib.h>

/**
 * @struct Musica
 * @brief Structure to store information about a Musica
 */
typedef struct Musica Musica;

/**
 * @brief Creates a new Musica
 *
 * @return A pointer to the newly created `Musica` structure
 */
Musica *newMusica();

/**
 * @brief Gets the Id of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The Id of the Musica
 */
gint getId(Musica *m);

/**
 * @brief Gets the title of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The title of the Musica
 */
gchar* getTitle(Musica *m);

/**
 * @brief Gets the artist Id of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The artist Id of the Musica
 */
gint* getArtistId_m(Musica *m);

/**
 * @brief Gets the album Id of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The album Id of the Musica
 */
gint getAlbumId_m(Musica *m);

/**
 * @brief Gets the duration of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The duration of the Musica
 */
gint getDuration(Musica *m);

/**
 * @brief Gets the genre of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The genre of the Musica
 */
gchar* getGenre(Musica *m);

/**
 * @brief Gets the number of Reproductions of the Musica
 * 
 * @param m Pointer to the `Musica` structure
 * @return The number of reproductions of the Musica
 */
gint getReproducoes(Musica *m);

/**
 * @brief Gets the year of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @return The year of the Musica
 */
gint getNumeroArtistas(Musica *m);

/**
 * @brief Sets the Id of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param id The new Id of the Musica
 */
void setId(Musica *m, gint id);

/**
 * @brief Sets the title of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param title The new title of the Musica
 */
void setTitle(Musica *m, const gchar *title);

/**
 * @brief Sets the artist Id of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param artist_id The new artist Id of the Musica
 */
void setArtistId_m(Musica *m, gint *artist_id, size_t length);

/**
 * @brief Sets the album Id of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param album_id The new album Id of the Musica
 */
void setAlbumID_m(Musica *m, gint album_id);

/**
 * @brief Sets the duration of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param duration The new duration of the Musica
 */
void setDuration(Musica *m, const gchar *duration);

/**
 * @brief Sets the genre of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param genre The new genre of the Musica
 */
void setGenre(Musica *m, const gchar *genre);


/**
 * @brief Sets the reproducoes of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param reproducoes The new reproducoes of the Musica
 */

void setReproducoes(Musica *m, gint reproducoes);

/**
 * @brief Increments the reproducoes of the Musica
 *
 * @param m Pointer to the `Musica` structure
 */
void incrementReproducoes(Musica *m );

/**
 * @brief Sets the number of artistas of the Musica
 *
 * @param m Pointer to the `Musica` structure
 * @param artistas The new number of artistas of the Musica
 */
void setNumeroArtistas(Musica *m, gint artistas);

/**
 * @brief Frees the memory allocated for a Musica
 *
 * @param m Pointer to the `Musica` structure to be freed
 */
void destroyMusica(Musica *m);

#endif // MUSICA_H