/**
 * @file tela_inicial.h
 * @brief Definition of functions that enable user interaction.
 */

#ifndef TELA_INICIAL_H
#define TELA_INICIAL_H

#include <ncurses.h>
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <stdlib.h>
#include <glib.h>

#include "../include/interativo/queries.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/gestores/gestor_musica.h"
#include "../include/gestores/gestorArtista.h"
#include "../include/gestores/gestor_albums.h"
#include "../include/gestores/gestor_historico.h"
#include "../include/my_recomendador.h"
#include "../include/Queries.h"
#include "../include/Output.h"
#include "../include/utils.h"

/**
 * @brief Displays the initial screen of the application, allowing the user to select
 * the dataset to be used and the commands file to be executed.
 */
void tela_inicial();

#endif // TELA_INICIAL_H