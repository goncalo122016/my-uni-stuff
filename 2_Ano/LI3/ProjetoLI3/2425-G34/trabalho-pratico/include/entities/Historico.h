/**
 * @file Historico.h
 * @brief Header file for the `Historico` entity.
 * 
 * This file contains the definition of the `Historico` structure and the 
 * declarations of functions to manipulate `Historico` objects.
 */

#ifndef HISTORICO_H
#define HISTORICO_H

#include <glib.h>

/**
 * @struct Historico
 * @brief Structure to store information about a Historico
 * 
 */
typedef struct Historico Historico;

/**
 * @brief Creates a new Historico
 *
 * @return A pointer to the newly created `Historico` structure
 */
Historico* newHistorico();

/**
 * @brief Frees the memory allocated for a Historico
 *
 * @param h Pointer to the `Historico` structure to be freed
 */
void deleteHistorico(Historico *h);

/**
 * @brief Gets the Id of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @return The Id of the Historico
 */
gint Historico_getId(Historico *h);

/**
 * @brief Gets the User_Id of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @return The User_Id of the Historico
 */
gint Historico_getUser_Id(Historico *h);

/**
 * @brief Gets the Musica_Id of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @return The Musica_Id of the Historico
 */
gint Historico_getMusica_Id(Historico *h);

/**
 * @brief Gets the date of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @return The date of the Historico
 */
gchar* Historico_getData(Historico *h);

/**
 * @brief Gets the hour of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @return The hour of the Historico
 */
gchar* Historico_getHora(Historico *h);

/**
 * @brief Gets the Duracao of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @return The Duracao of the Historico
 */
gint Historico_getDuracao(Historico *h);

/**
 * @brief Sets the Id of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @param Id The new Id of the Historico
 */
void setHistoricoId(Historico *h, gint Id);

/**
 * @brief Sets the Musica_Id of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @param Musica_Id The new Musica_Id of the Historico
 */
void setHistoricoUser_Id(Historico *h, gint User_Id);

/**
 * @brief Sets the Musica_Id of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @param Musica_Id The new Musica_Id of the Historico
 */
void setHistoricoMusica_Id(Historico *h, gint Musica_Id);

/**
 * @brief Sets the date of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @param Data The new date of the Historico
 */
void setHistoricoData(Historico *h, gchar *Data);

/**
 * @brief Sets the hour of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @param Hora The new hour of the Historico
 */
void setHistoricoHora(Historico *h, gchar *Hora);

/**
 * @brief Sets the duration of the Historico
 *
 * @param h Pointer to the `Historico` structure
 * @param Duracao The new duration of the Historico
 */

void setHistoricoDuracao(Historico *h, gchar *Duracao);

#endif // HISTORICO_H