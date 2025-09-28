#include "../../include/entities/Artista.h"

#include <glib.h>

// Struct declaration for Artista
struct Artista {
  gint Id;
  gchar *Name;
  double RecipePerStream;
  gint *IdConstituent;
  gchar *Country;
  gint Type;
  gint Albuns;
  gint Discografia;
  gint *DiscografiaSemanal;
  gint numeroConstituents;
};

Artista *newArtist() {
  Artista *a = malloc(sizeof(Artista));
  a->Id = 0;
  a->Name = NULL;
  a->RecipePerStream = 0;
  a->IdConstituent =
      malloc(sizeof(gint) * 5);  // Adjust size as needed
    for (int i = 0; i < 5; i++) {
      a->IdConstituent[i] = 0;
    }
  a->Country = NULL;
  a->Type = 0;
  a->Albuns = 0;
  a->Discografia = 0;
  a->DiscografiaSemanal = malloc(sizeof(gint) * NUMBER_WEEKS);
  for (int i = 0; i < NUMBER_WEEKS; i++) {
    a->DiscografiaSemanal[i] = 0;
  }
  a->numeroConstituents = 0;
  return a;
}

// Getters

gint getArtistId(Artista *a) { return a->Id; }

gchar *getArtistName(Artista *a) { return g_strdup(a->Name); }

double getArtistRecipePerStream(Artista *a) {
  return a->RecipePerStream;
}

gint *getArtistIdConstituent(Artista *a) {
  size_t numConstituents = getNumeroConstituents(a);

  gint *duplicated = malloc(sizeof(gint) * numConstituents);
  memcpy(duplicated, a->IdConstituent, numConstituents * sizeof(gint));
  return duplicated;
}

gchar *getArtistCountry(Artista *a) { return g_strdup(a->Country); }

gint getArtistType(Artista *a) { return a->Type; }

gint getAlbuns(Artista *a) { return a->Albuns; }

gint getArtistDisco(Artista *a) { return a->Discografia; }

gint getDiscoSemanal(Artista *a, gint semana) {
  return a->DiscografiaSemanal[semana];
}

gint getNumeroConstituents(Artista *a) { return a->numeroConstituents; }

// Setters

void setArtistId(Artista *a, const gint id) { a->Id = id; }

void setArtistName(Artista *a, const gchar *name) {
  g_free(a->Name);
  a->Name = g_strdup(name);
}

void setArtistRecipePerStream(Artista *a, double recipePerStream) {
  a->RecipePerStream = recipePerStream;
}

void setArtistIdConstituent(Artista *a, gint *idConstituent,
                            size_t length) {
  g_free(a->IdConstituent);

  if (length > 0) {
    a->IdConstituent = g_malloc(length * sizeof(gint));
    memcpy(a->IdConstituent, idConstituent, length * sizeof(gint));
  } else {
    a->IdConstituent = NULL;
  }
}

void setArtistCountry(Artista *a, const gchar *country) {
  g_free(a->Country);
  a->Country = g_strdup(country);
}

void setArtistType(Artista *a, gint type) { a->Type = type; }

void setNumeroConstituents(Artista *a, gint numero) {
  a->numeroConstituents = numero;
}

void incrementAlbuns(Artista *a) { a->Albuns++; }

void aumentaDiscografia(Artista *a, gint aumento) {
  a->Discografia += aumento;
}

void aumentaDiscoSemanal(Artista *a, gint aumento, gint semana) {
  a->DiscografiaSemanal[semana] += aumento;
}

void removeArtist(Artista *a) {
  if (!a) return;
  if (a->Name) g_free(a->Name);
  if (a->IdConstituent) g_free(a->IdConstituent);
  if (a->Country) g_free(a->Country);
  if (a->DiscografiaSemanal) free(a->DiscografiaSemanal);
  free(a);
}
