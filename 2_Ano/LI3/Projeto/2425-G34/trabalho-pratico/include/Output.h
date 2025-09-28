/**
 * @file Output.h
 * @brief Header file for output file operations.
 * 
 * This file contains the declaration of the function used to create and write 
 * to an output file.
 */
#ifndef OUTPUT_H
#define OUTPUT_H

#include <glib.h>

/**
 * @brief Creates and writes to the output file
 *
 * @param filename The name of the file to be created and written to
 * @param output The content to be written to the file
 */
void write_output_file(const gchar *filename, const gchar *output);

#endif // OUTPUT_H