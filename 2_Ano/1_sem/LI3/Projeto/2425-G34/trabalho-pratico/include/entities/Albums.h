/**
 * @file Albums.h
 * @brief Header file for the Album entity.
 * 
 * This file contains the definition of the Album structure and the 
 * declarations of functions to manipulate Album objects.
 */

#ifndef ALBUNS_H
#define ALBUNS_H

#include <glib.h>

/**
 * @struct Album
 * @brief Structure to store information about an Album
 * 
 */
typedef struct Album Album;

/**
 * 
 * @brief Inicializa um novo Álbum
 *
 * @return Um ponteiro para a estrutura Album recém-criada
 */
Album* Album_new();

/**
 * @brief Liberta a memória alocada para um Álbum
 *
 * @param a Ponteiro para a estrutura Album a ser liberada
 */
void deleteAlbum(Album *a);

/**
 * @brief Obtém o Id do Álbum
 *
 * @param a Ponteiro para a estrutura Album
 * @return O Id do Álbum
 */
gint Album_getId(Album *a);

/**
 * @brief Obtém o título do Álbum
 *
 * @param a Ponteiro para a estrutura Album
 * @return O título do Álbum
 */
gchar* Album_getTitle(Album *a);

/**
 * @brief Obtém o Id do artista do Álbum
 *
 * @param a Ponteiro para a estrutura Album
 * @return O Id do artista do Álbum
 */
gint* Album_getArtist_Id(Album *a);

/**
 * @brief Obtém o número de artistas do Álbum
 *
 * @param a Ponteiro para a estrutura Album
 * @return O número de artistas do Álbum
 */
gint Album_getNumeroArtistas(Album *a);

/**
 * @brief Sets the ID of an Album.
 *
 * @param a Pointer to the Album structure.
 * @param Id The new ID to assign to the Album.
 */
void setAlbumId(Album *a, gint Id);

/**
 * @brief Sets the title of an Album.
 *
 * @param a Pointer to the Album structure.
 * @param Title The new title to assign to the Album.
 */
void setAlbumTitle(Album *a, gchar *Title);

/**
 * @brief Sets the artist ID(s) of an Album.
 *
 * This function assigns a new set of artist IDs to the specified Album structure.
 *
 * @param a Pointer to the Album structure.
 * @param Artist_Id Pointer to an array of new artist IDs to assign to the Album.
 * @param length The number of artist IDs in the array.
 */
void setAlbumArtist_Id(Album *a, gint *Artist_Id, size_t length);

/**
 * @brief Sets the number of artists for an Album.
 *
 * @param a Pointer to the Album structure.
 * @param numero The new number of artists for the Album.
 */
void setAlbumNumeroArtistas(Album *a, gint numero);

#endif // ALBUNS_H