/**
 * @file utils.h
 * @brief Utility functions.
 * 
 * This file contains utility functions that are used throughout the project.
 * These functions are used to perform various operations, such as string manipulation,
 * time conversion, and other general-purpose tasks.
 */
#ifndef UTILS_H
#define UTILS_H

#include <glib.h>

/**
 * @brief Removes a set of characters from a string.
 * 
 * @param str The original string.
 * @param remove_chars The characters to be removed from the string.
 * @return A new string with the specified characters removed.
 */
gchar *remove_chars(gchar *str, gchar *remove_chars);

/**
 * @brief Calculates the difference in seconds from a given date and time string.
 * 
 * @param data_hora The date and time string to be evaluated.
 * @return The difference in seconds.
 */
long calcular_diferenca_segundos(const char *data_hora);

/**
 * @brief Converts a time string to seconds.
 * 
 * @param str The time string to be converted.
 * @return The equivalent time in seconds.
 */
gint time_string_to_seconds(const gchar *str);

/**
 * @brief Validates if a key exists in a hashtable.
 * 
 * @param hashtable The hashtable to be checked.
 * @param key The key to be validated.
 * @return TRUE if the key exists in the hashtable, FALSE otherwise.
 */
gboolean validate_key_in_hashtable(GHashTable *hashtable, gint key);

/**
 * Counts the number of digits in an integer.
 *
 * @param numero The integer whose digits are to be counted.
 * @return The number of digits in the given integer.
 */
int contar_digitos(int numero);

/**
 * Creates a string consisting of 'n' zeros.
 *
 * @param n The number of zeros in the string.
 * @return A string containing 'n' zeros.
 */
char *createZerosString(int n);

/**
 * @brief Converts all characters in a string to lowercase.
 * 
 * @param s1 The input string to be converted.
 * @param s2 The output string where the lowercase result will be stored.
 */
void minusculo(gchar s1[], gchar s2[]);

/**
 * @brief Converts seconds to a time string in the format "hh:mm:ss".
 * 
 * @param total_seconds The total number of seconds to be converted.
 * @return A string representing the time in "hh:mm:ss" format.
 */
gchar *time_seconds_to_hms(gint total_seconds);

/**
 * @brief Gets the index of the maximum value in an array.
 * 
 * @param array The array to be analyzed.
 * @param length The length of the array.
 * @return The index of the maximum value in the array.
 */
gint get_max_index(const gint *array, gsize length);

/**
 * @brief Compares two date strings.
 * 
 * @param date1 The first date string.
 * @param date2 The second date string.
 * @return A negative value if date1 is earlier, zero if they are equal, and a positive value if date1 is later.
 */
int compare_dates(const char *date1, const char *date2);

#endif // UTILS_H