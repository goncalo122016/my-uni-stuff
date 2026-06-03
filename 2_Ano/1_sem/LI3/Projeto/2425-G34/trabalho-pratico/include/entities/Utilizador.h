/**
 * @file Utilizador.h
 * @brief Header file for the `Utilizador` entity.
 * 
 * This file contains the definition of the `Utilizador` structure and the 
 * declarations of functions to manipulate `Utilizador` objects.
 */

#ifndef UTILIZADOR_H
#define UTILIZADOR_H

#include <glib.h>

/**
 * @struct Utilizador
 * @brief Structure to store information about a `Utilizador`
 * 
 */
typedef struct Utilizador Utilizador;

/**
 * @brief Creates a new `Utilizador`
 *
 * @return A pointer to the newly created `Utilizador` structure
 */
Utilizador* newUtilizador();

/**
 * @brief Parses a string of information to create a `Utilizador` structure
 *
 * @param information The string containing the information
 * @return A pointer to the created `Utilizador` structure
 */
gint calculateIdade(const gchar *dataNascimento);

/**
 * @brief Frees the memory allocated for a `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure to be freed
 */
void deleteUtilizador(Utilizador *u);

/**
 * @brief Gets the username(ID) of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The username of the `Utilizador`
 */
gint getUsername(Utilizador *u);

/**
 * @brief Gets the email of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The email of the `Utilizador`
 */
gchar* getEmail(Utilizador *u);

/**
 * @brief Gets the first name of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The first name of the `Utilizador`
 */
gchar* getFirstName(Utilizador *u);

/**
 * @brief Gets the last name of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The last name of the `Utilizador`
 */
gchar* getLastName(Utilizador *u);

/**
 * @brief Gets the age of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The age of the `Utilizador`
 */
gint getAge(Utilizador *u);

/**
 * @brief Gets the country of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The country of the `Utilizador`
 */
gchar* getCountry(Utilizador *u);

/**
 * @brief Gets the liked musics of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return An array of strings containing the liked musics of the `Utilizador`
 */
gint* getLikedMusics(Utilizador *u);

/**
 * @brief Gets the number of liked musics of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @return The number of liked musics of the `Utilizador`
 */
gint getNumeroMusicas(Utilizador *u);

/**
 * @brief Sets the username(ID) of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param username The new username of the `Utilizador`
 */
void setUsername(Utilizador *u, gint username);

/**
 * @brief Sets the email of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param email The new email of the `Utilizador`
 */
void setEmail(Utilizador *u, const gchar *email);

/**
 * @brief Sets the first name of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param firstName The new first name of the `Utilizador`
 */
void setFirstName(Utilizador *u, const gchar *firstName);

/**
 * @brief Sets the last name of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param lastName The new last name of the `Utilizador`
 */
void setLastName(Utilizador *u, const gchar *lastName);

/**
 * @brief Sets the age of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param dateOfBirth The new age of the `Utilizador`
 */
void setAge(Utilizador *u, const gchar *dateOfBirth);

/**
 * @brief Sets the liked musics of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param likedMusics An array of integers containing the new liked musics of the `Utilizador`
 * @param length The number of elements in the likedMusics array
 */
void setLikedMusics(Utilizador *u, gint* likedMusics, size_t length);

/**
 * @brief Sets the country of the Utilizador
 *
 * @param u Pointer to the Utilizador structure
 * @param country The new country of the Utilizador
 */
void setCountry(Utilizador *u, const gchar *country);

/**
 * @brief Sets the liked musics of the `Utilizador`
 *
 * @param u Pointer to the `Utilizador` structure
 * @param likedMusics An array of integers containing the new liked musics of the `Utilizador`
 * @param length The number of elements in the likedMusics array
 */
void setNumeroMusicas(Utilizador *u, gint numero);

#endif // UTILIZADOR_H
