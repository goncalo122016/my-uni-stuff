#include "../../include/entities/Musica.h"

#include <glib.h>
#include <stdio.h>

struct Musica {
  gint id;
  gchar *title;
  gint *artist_id;
  gint album_id;
  gchar *duration;
  gchar *genre;
  gint year;
  gint reproducoes;
  gint numeroArtistas;
};

// Create

Musica *newMusica() {
  Musica *m = malloc(sizeof(Musica));
  if (m == NULL) {
    return NULL;
  }
  m->id = 0;
  m->title = NULL;
  m->artist_id = NULL;
  m->album_id = 0;
  m->duration = NULL;
  m->genre = NULL;
  m->year = 0;
  m->reproducoes = 0;
  return m;
}

// Getters

gint getId(Musica *m) { return m->id; }

gchar *getTitle(Musica *m) { return g_strdup(m->title); }

gint *getArtistId_m(Musica *m) {
  size_t numArtist = getNumeroArtistas(m);

  gint *duplicated = malloc(sizeof(gint) * numArtist);
  memcpy(duplicated, m->artist_id, numArtist * sizeof(gint));
  return duplicated;
}

gint getAlbumId_m(Musica *m) { return m->album_id; }

gint getDuration(Musica *m) {
  // Check if the duration is NULL
  if (m->duration == NULL) {
    printf("Error: Duration string is NULL.\n");
    return 0;
  }

  gchar **tempo = g_strsplit(m->duration, ":", 3);

  gint res = 0;
  gint horas = 0, min = 0, sec = 0;

  if (tempo[0] != NULL) horas = g_ascii_strtoll(tempo[0], NULL, 10);
  if (tempo[1] != NULL) min = g_ascii_strtoll(tempo[1], NULL, 10);
  if (tempo[2] != NULL) sec = g_ascii_strtoll(tempo[2], NULL, 10);

  res = horas * 3600 + min * 60 + sec;

  g_strfreev(tempo);

  return res;
}

gchar *getGenre(Musica *m) { return g_strdup(m->genre); }

gint getReproducoes(Musica *m) { return m->reproducoes; }

gint getNumeroArtistas(Musica *m) { return m->numeroArtistas; }

// Setters

void setId(Musica *m, gint id) { m->id = id; }

void setTitle(Musica *m, const gchar *title) {
  g_free(m->title);
  m->title = g_strdup(title);
}

void setArtistId_m(Musica *m, gint *artist_id, size_t length) {
  g_free(m->artist_id);

  if (length > 0) {
    m->artist_id = g_malloc(length * sizeof(gint));
    memcpy(m->artist_id, artist_id, length * sizeof(gint));
  } else {
    m->artist_id = NULL;
  }
}

void setAlbumID_m(Musica *m, gint album_id) { m->album_id = album_id; }

void setDuration(Musica *m, const gchar *duration) {
  g_free(m->duration);
  m->duration = g_strdup(duration);
}

void setGenre(Musica *m, const gchar *genre) {
  g_free(m->genre);
  m->genre = g_strdup(genre);
}

void setReproducoes(Musica *m, gint reproducoes) {
  m->reproducoes = reproducoes;
}

void incrementReproducoes(Musica *m) { m->reproducoes++; }

void setNumeroArtistas(Musica *m, gint artistas) {
  m->numeroArtistas = artistas;
}

// Destroy

void destroyMusica(Musica *m) {
  if (m) {
    g_free(m->title);
    g_free(m->artist_id);
    g_free(m->duration);
    g_free(m->genre);
    free(m);
  }
}