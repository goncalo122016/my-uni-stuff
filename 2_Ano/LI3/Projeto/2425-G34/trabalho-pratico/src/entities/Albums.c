#include "../../include/entities/Albums.h"

#include <glib.h>

// Definição da estrutura Album
struct Album {
  gint Id;
  gchar *Title;
  gint *Artist_Id;
  gchar *Year;
  gint numeroArtistas;
};

// Função que inicializa um Álbum
Album *Album_new() {
  Album *a = malloc(sizeof(Album));
  if (a == NULL) {
    return NULL;
  }
  a->Id = 0;
  a->Title = NULL;
  a->Artist_Id = NULL;
  a->numeroArtistas = 0;
  return a;
}

// Função para libertar um Álbum
void deleteAlbum(Album *a) {
  if (a == NULL) {
    return;
  }
  if (a->Title) g_free(a->Title);
  if (a->Artist_Id) g_free(a->Artist_Id);
  g_free(a);
}

// Getters

gint Album_getId(Album *a) { return a->Id; }

gchar *Album_getTitle(Album *a) { return g_strdup(a->Title); }

gint *Album_getArtist_Id(Album *a) {
  size_t numArtist = Album_getNumeroArtistas(a);

  gint *duplicated = malloc(sizeof(gint) * numArtist);
  memcpy(duplicated, a->Artist_Id, numArtist * sizeof(gint));
  return duplicated;
}

gint Album_getNumeroArtistas(Album *a) { return a->numeroArtistas; }

// Setters

void setAlbumId(Album *a, gint Id) { a->Id = Id; }

void setAlbumTitle(Album *a, gchar *Title) {
  g_free(a->Title);
  a->Title = g_strdup(Title);
}

void setAlbumArtist_Id(Album *a, gint *Artist_Id, size_t length) {
  g_free(a->Artist_Id);

  if (length > 0) {
    a->Artist_Id = g_malloc(length * sizeof(gint));
    memcpy(a->Artist_Id, Artist_Id, length * sizeof(gint));
  } else {
    a->Artist_Id = NULL;
  }
}

void setAlbumNumeroArtistas(Album *a, gint numero) {
  a->numeroArtistas = numero;
}
