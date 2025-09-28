#include <glib.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "../../include/entities/Utilizador.h"

// Definição de Utilizador
struct Utilizador {
    gint Username;
    gchar* Email;
    gchar* FirstName;
    gchar* LastName;
    gchar* Country;
    gint* LikedMusics;
    gint Age; 
    gint numeroMusicas;
};

gint calculateIdade(const gchar *dataNascimento) {
    if (dataNascimento == NULL) {
        return -1;
    }

    // Extrair o ano, mês e dia da string no formato "yyyy/mm/dd"
    gint ano, mes, dia;
    if (sscanf(dataNascimento, "%d/%d/%d", &ano, &mes, &dia) != 3) {
        // Se a string não estiver no formato correto
        printf("Erro: Formato de data inválido. Use 'yyyy/mm/dd'.\n");
        return -1;
    }

    // Data de referência: 09/09/2024
    gint refAno = 2024;
    gint refMes = 9;
    gint refDia = 9;

    // Calcular a idade
    gint idade = refAno - ano;

    // Verificar se a pessoa ainda não fez aniversário neste ano
    if (mes > refMes || (mes == refMes && dia > refDia)) {
        idade--;
    }

    return idade;
}

// Função para criar um novo Utilizador com valores iniciais vazios
Utilizador* newUtilizador() {
    Utilizador *u = malloc(sizeof(Utilizador));
    if (u == NULL) {
        return NULL;
    }

    u->Username = 0;
    u->Email = NULL;
    u->FirstName = NULL;
    u->LastName = NULL;
    u->Country = NULL;
    u->LikedMusics = NULL;
    u->Age = -1;
    u->numeroMusicas = 0;

    return u;
}

void deleteUtilizador(Utilizador *u) {
    if (u == NULL) return;
    g_free(u->Email);
    g_free(u->FirstName);
    g_free(u->LastName);
    g_free(u->Country);
    g_free(u->LikedMusics);
    free(u);
}

// Getters

gint getUsername(Utilizador *u) {
    return u->Username;
}

gchar* getEmail(Utilizador *u) {
    return g_strdup(u->Email);
}

gchar* getFirstName(Utilizador *u) {
    return g_strdup(u->FirstName);
}

gchar* getLastName(Utilizador *u) {
    return g_strdup(u->LastName);
}

gint getAge(Utilizador *u) {
    return u->Age;
}

gchar* getCountry(Utilizador *u) {
    return g_strdup(u->Country);
}

gint* getLikedMusics(Utilizador *u) {
    size_t numLikes = getNumeroMusicas(u);

    gint *duplicated = malloc(numLikes * sizeof(gint));
    memcpy(duplicated, u->LikedMusics, numLikes * sizeof(gint));

    return duplicated;
}

gint getNumeroMusicas(Utilizador *u){ return u->numeroMusicas; }

// Setters

void setUsername(Utilizador *u, gint username) {
    u->Username = username;
}

void setEmail(Utilizador *u, const gchar *email) {
    g_free(u->Email);
    u->Email = g_strdup(email);
}

void setFirstName(Utilizador *u, const gchar *firstName) {
    g_free(u->FirstName);
    u->FirstName = g_strdup(firstName);
}

void setLastName(Utilizador *u, const gchar *lastName) {
    g_free(u->LastName);
    u->LastName = g_strdup(lastName);
}

void setAge(Utilizador *u, const gchar *dateOfBirth) {
    u->Age = calculateIdade(dateOfBirth);
}

void setCountry(Utilizador *u, const gchar *country) {
    g_free(u->Country);
    u->Country = g_strdup(country);
}

void setLikedMusics(Utilizador *u, gint* likedMusics, size_t length) {
    g_free(u->LikedMusics);

  if (length > 0) {
    u->LikedMusics = g_malloc(length * sizeof(gint)); 
    memcpy(u->LikedMusics, likedMusics, length * sizeof(gint));
  } else {
    u->LikedMusics = NULL; 
  }
}

void setNumeroMusicas(Utilizador *u, gint numero){
    u->numeroMusicas = numero;
}
