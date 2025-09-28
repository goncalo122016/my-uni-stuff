#include <ctype.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "../include/Output.h"
#include "../include/gestores/gestorUtilizador.h"
#include "../include/utils.h"

// Função para validar o formato do ID do utilizador
gboolean validate_id_utilizador(const gchar *id) {
  if (id == NULL || strlen(id) != 8 || id[0] != 'U') {
    return FALSE;
  }
  for (int i = 1; i < 8; i++) {
    if (!isdigit(id[i])) {
      return FALSE;
    }
  }
  return TRUE;
}

// Função auxiliar para verificar se um ano é bissexto
static gboolean isLeapYear(int year) {
  return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

// Função auxiliar para validar o formato da data (yyyy/mm/dd)
static gboolean isDateFormatValid(const gchar *date) {
  if (strlen(date) != 10) {
    return FALSE;
  }

  gchar **date_parts = g_strsplit(date, "/", 3);

  if (g_strv_length(date_parts) != 3) {
    g_strfreev(date_parts);
    return FALSE;
  }

  if (strlen(date_parts[0]) != 4 || strlen(date_parts[1]) != 2 ||
      strlen(date_parts[2]) != 2) {
    g_strfreev(date_parts);
    return FALSE;
  }

  for (gint i = 0; i < 3; i++) {
    for (gsize j = 0; j < strlen(date_parts[i]); j++) {
      if (!g_ascii_isdigit(date_parts[i][j])) {
        g_strfreev(date_parts);
        return FALSE;
      }
    }
  }

  g_strfreev(date_parts);

  return TRUE;
}

// Função auxiliar para verificar se o mês e o dia são válidos
static gboolean isMonthAndDayValid(int year, int month, int day) {
  if (month < 1 || month > 12) {
    return FALSE;
  }

  // Número de dias em cada mês (índice 0 é ignorado para simplificar a
  // indexação)
  gint daysInMonth[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  // Ajusta o número de dias de fevereiro se for um ano bissexto
  if (month == 2 && isLeapYear(year)) {
    daysInMonth[2] = 29;
  }

  if (day < 1 || day > daysInMonth[month]) {
    return FALSE;
  }

  return TRUE;
}

// Função auxiliar para verificar se a data não é futura
static gboolean isDateNotInFuture(int year, int month, int day) {
  // Data a ter em conta dada pelo Guião é "2024/09/09"
  gint currentYear = 2024;
  gint currentMonth = 9;
  gint currentDay = 9;

  if (year > currentYear) {
    return FALSE;
  } else if (year == currentYear) {
    if (month > currentMonth) {
      return FALSE;
    } else if (month == currentMonth && day > currentDay) {
      return FALSE;
    }
  }

  return TRUE;
}

// Função principal para validar a data completa
static gboolean isValidDate(const gchar *datainicial) {
  if (!isDateFormatValid(datainicial)) {
    return FALSE;
  }

  gchar **date = g_strsplit(datainicial, "/", 3);

  // Converter as partes para inteiros
  int year = g_ascii_strtoll(date[0], NULL, 10);
  int month = g_ascii_strtoll(date[1], NULL, 10);
  int day = g_ascii_strtoll(date[2], NULL, 10);

  g_strfreev(date);

  if (!isMonthAndDayValid(year, month, day)) {
    return FALSE;
  }

  if (!isDateNotInFuture(year, month, day)) {
    return FALSE;
  }

  return TRUE;
}

// Função para validar um email
static gboolean isValidEmail(const gchar *email) {
  // Expressão padrão para validar o formato do e-mail conforme as regras
  // fornecidas no Guião
  const gchar *pattern = "^[a-z0-9]+@[a-z]+\\.[a-z]{2,3}$";

  GRegex *regra = g_regex_new(pattern, 0, 0, NULL);
  gboolean match = g_regex_match(regra, email, 0, NULL);

  g_regex_unref(regra);

  return match;
}

// Função para validar a Subscrição
static gboolean isValidSubscription(const gchar *subscription) {
  return g_ascii_strcasecmp(subscription, "Normal") == 0 ||
         g_ascii_strcasecmp(subscription, "Premium") == 0;
}

static gboolean areIdsAllValid(gint *likedMusics, GHashTable *musicas_table, gint length) {
  for (gint i = 0; i < length; i++) {
    if (!(g_hash_table_contains(musicas_table, &likedMusics[i]))) {
      return FALSE;
    }
  }
  return TRUE;
}

// Função para criar uma struct Utilizador a partir de uma string de informação
// de um ficheiro
Utilizador *parse_Utilizador(gchar *information, GHashTable *musictable,
                                    GenreLikes ***gL) {
  gchar *clean_information = remove_chars(information, "\"");
  gchar **tokens = g_strsplit(clean_information, ";", 8);
  g_free(clean_information);
  if (g_strv_length(tokens) != 8) {
    g_strfreev(tokens);
    return NULL;
  }

  // Criar um novo utilizador vazio
  Utilizador *u = newUtilizador();
  if (!u) {
    g_strfreev(tokens);
    return NULL;
  }

  // Validação do ID do utilizador
  if (!validate_id_utilizador(tokens[0])) {
    g_strfreev(tokens);
    deleteUtilizador(u);
    return NULL;
  }
  gint id = g_ascii_strtoll(tokens[0] + 1, NULL, 10);
  setUsername(u, id);

  // Validação do Email
  if (!isValidEmail(tokens[1])) {
    g_strfreev(tokens);
    deleteUtilizador(u);
    return NULL;
  }

  setEmail(u, tokens[1]);
  setFirstName(u, tokens[2]);
  setLastName(u, tokens[3]);

  // Validação da Data de Nascimento
  if (!isValidDate(tokens[4])) {
    g_strfreev(tokens);
    deleteUtilizador(u);
    return NULL;
  }

  setAge(u, tokens[4]);

  setCountry(u, tokens[5]);

  // Validação do tipo de subscrição
  if (!isValidSubscription(tokens[6])) {
    g_strfreev(tokens);
    deleteUtilizador(u);
    return NULL;
  }

  // Processar as músicas gostadas
  if (g_strcmp0(tokens[7], "[]") == 0) {
    setLikedMusics(u, NULL, 0);
  } else {
    gchar *clean_brackets = remove_chars(tokens[7], "[]\' ");
    gchar **likedMusics = g_strsplit(clean_brackets, ",", -1);
    gint length = g_strv_length(likedMusics);
    gint *idsMusics = g_new(gint, length);
    int index;
    for (index = 0; likedMusics[index]; index++){
      idsMusics[index] = g_ascii_strtoll(likedMusics[index] + 1, NULL, 10);
    }
    g_free(clean_brackets);

    if (!areIdsAllValid(idsMusics, musictable, index)) {
      g_strfreev(tokens);
      deleteUtilizador(u);
      g_strfreev(likedMusics);
      g_free(idsMusics);
      return NULL;
    }

    gint idade = getAge(u) - 17;

    for (gint i = 0; i < index; i++) {
      Musica *m = g_hash_table_lookup(musictable, &idsMusics[i]);
      gchar *genre = getGenre(m);
      if (g_strcmp0(genre, "Metal") == 0) {
        aumentaLikes(gL[idade][0], 1);
        if (getLikes_gL(gL[idade][0]) == 1) {
          setGenre_gL(gL[idade][0], genre);
        }
      } else if (g_strcmp0(genre, "Reggae") == 0) {
        aumentaLikes(gL[idade][1], 1);
        if (getLikes_gL(gL[idade][1]) == 1) {
          setGenre_gL(gL[idade][1], genre);
        }
      } else if (g_strcmp0(genre, "Jazz") == 0) {
        aumentaLikes(gL[idade][2], 1);
        if (getLikes_gL(gL[idade][2]) == 1) {
          setGenre_gL(gL[idade][2], genre);
        }
      } else if (g_strcmp0(genre, "Pop") == 0) {
        aumentaLikes(gL[idade][3], 1);
        if (getLikes_gL(gL[idade][3]) == 1) {
          setGenre_gL(gL[idade][3], genre);
        }
      } else if (g_strcmp0(genre, "Classical") == 0) {
        aumentaLikes(gL[idade][4], 1);
        if (getLikes_gL(gL[idade][4]) == 1) {
          setGenre_gL(gL[idade][4], genre);
        }
      } else if (g_strcmp0(genre, "Electronic") == 0) {
        aumentaLikes(gL[idade][5], 1);
        if (getLikes_gL(gL[idade][5]) == 1) {
          setGenre_gL(gL[idade][5], genre);
        }
      } else if (g_strcmp0(genre, "Country") == 0) {
        aumentaLikes(gL[idade][6], 1);
        if (getLikes_gL(gL[idade][6]) == 1) {
          setGenre_gL(gL[idade][6], genre);
        }
      } else if (g_strcmp0(genre, "Blues") == 0) {
        aumentaLikes(gL[idade][7], 1);
        if (getLikes_gL(gL[idade][7]) == 1) {
          setGenre_gL(gL[idade][7], genre);
        }
      } else if (g_strcmp0(genre, "Hip Hop") == 0) {
        aumentaLikes(gL[idade][8], 1);
        if (getLikes_gL(gL[idade][8]) == 1) {
          setGenre_gL(gL[idade][8], genre);
        }
      } else if (g_strcmp0(genre, "Rock") == 0) {
        aumentaLikes(gL[idade][9], 1);
        if (getLikes_gL(gL[idade][9]) == 1) {
          setGenre_gL(gL[idade][9], genre);
        }
      } else {
        return NULL;
      }
      g_free(genre);
    }
    setLikedMusics(u, idsMusics, index);
    setNumeroMusicas(u, index);
    g_strfreev(likedMusics);
    g_free(idsMusics);
  }

  g_strfreev(tokens);
  return u;
}