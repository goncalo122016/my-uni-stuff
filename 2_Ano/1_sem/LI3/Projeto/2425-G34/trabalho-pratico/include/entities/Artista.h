/**
 * @file Artista.h
 * @brief Header file for the `Artista` entity.
 * 
 * This file contains the definition of the `Artista` structure and the 
 * declarations of functions to manipulate `Artista` objects.
 */
#ifndef ARTISTA_H
#define ARTISTA_H

#define NUMBER_WEEKS 327

#include <glib.h>

/**
 * @struct Artista
 * @brief Structure to store information about an Artista
 * 
 */
typedef struct Artista Artista;

/**
 * @brief Creates a new Artista
 *
 * @return A pointer to the newly created `Artista` structure
 */
Artista* newArtist();

/**
 * @brief Gets the Id of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The Id of the Artista
 */
gint getArtistId(Artista* a);


/**
 * @brief Gets the name of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The name of the Artista
 */
gchar* getArtistName(Artista* a);

/**
 * @brief Gets the recipe per stream of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The recipe per stream of the Artista
 */
double getArtistRecipePerStream(Artista* a);

/**
 * @brief Gets the Id constituent of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return An array of strings containing the Id constituent of the Artista
 */
gint* getArtistIdConstituent(Artista* a);


/**
 * @brief Gets the country of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The country of the Artista
 */
gchar* getArtistCountry(Artista* a);

/**
 * @brief Gets the type of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The type of the `Artista` (0 for individual, 1 for group)
 */
gint getArtistType(Artista* a);

/**
 * @brief Gets the number of albuns of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The number of albuns of the Artista
 */
gint getAlbuns(Artista *a);

/**
 * @brief Gets the discography of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The discography of the Artista
 */
gint getArtistDisco(Artista *a);

/**
 * @brief Gets the discography count for a specific week.
 *
 * @param a Pointer to the `Artista` structure
 * @param semana The specific week for which to retrieve the discography count.
 * @return The number of albums/songs released during the specified week.
 */
gint getDiscoSemanal(Artista *a, gint semana);

/**
 * @brief Gets the number of constituents of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @return The number of constituents of the Artista
 */
gint getNumeroConstituents(Artista *a);

/**
 * @brief Sets the Id of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param id The new Id of the Artista
 */
void setArtistId(Artista* a, gint id);

/**
 * @brief Sets the name of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param name The new name of the Artista
 */
void setArtistName(Artista* a, const gchar* name);

/**
 * @brief Sets the recipe per stream of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param recipePerStream The new recipe per stream of the Artista
 */
void setArtistRecipePerStream(Artista* a, double recipePerStream);

/**
 * @brief Sets the Id constituent of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param idConstituent An array of strings containing the new Id constituent of the Artista
 */
void setArtistIdConstituent(Artista *a, gint *idConstituent,size_t length);

/**
 * @brief Sets the country of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param country The new country of the Artista
 */
void setArtistCountry(Artista* a, const gchar* country);

/**
 * @brief Sets the type of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param type The new type of the Artista
 */
void setArtistType(Artista* a, gint type);

/**
 * @brief Sets the number of constituents of the Artista
 *
 * @param a Pointer to the `Artista` structure
 * @param numero The new number of constituents of the Artista
 */
void setNumeroConstituents(Artista *a, gint numero);

/**
 * @brief Increases the number of albuns for an artist.
 *
 * @param a Pointer to the `Artista` structure.
 */
void incrementAlbuns(Artista *a);

/**
 * @brief Increases the total discography count for an artist.
 *
 * @param a Pointer to the `Artista` structure.
 * @param aumento The amount by which to increase the discography count.
 */
void aumentaDiscografia(Artista *a, gint aumento);

/**
 * @brief Increases the weekly discography count for an artist.
 *
 * @param a Pointer to the `Artista` structure.
 * @param aumento The amount by which to increase the discography count for the specified week.
 * @param semana The specific week for which to update the discography count.
 */
void aumentaDiscoSemanal(Artista *a, gint aumento, gint semana);

/**
 * @brief Frees the memory allocated for an Artista
 *
 * @param a Pointer to the `Artista` structure to be freed
 */
void removeArtist(Artista *a);

#endif  // ARTISTA_H