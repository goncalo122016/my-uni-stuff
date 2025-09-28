/**
 * @file seleciona_dataset.h
 * @brief Definition of functions that enable user interaction.
 */

#ifndef SELECIONA_DATASET_H
#define SELECIONA_DATASET_H

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
 * @brief Selects the dataset to be used in the application.
 * 
 * @param dataset_directory The directory containing the dataset files.
 * @param commands_file_path The path to the commands file.
 */
void selecionar_dataset(char **dataset_directory, char **commands_file_path);

#endif // SELECIONA_DATASET_H