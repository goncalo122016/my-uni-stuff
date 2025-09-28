#include <glib.h>
#include "../../include/utils.h"
#include "../../include/entities/Historico.h"

struct Historico {
    gint Id;
    gint User_Id;
    gint Musica_Id;
    gchar *Data;
    gchar *Hora;
    gint Duracao;
};

// Create/Delete

Historico* newHistorico() {
    Historico *h = malloc(sizeof(Historico));
    if (h == NULL) {
        return NULL;
    }
    h->Id = 0;
    h->User_Id = 0;
    h->Musica_Id = 0;
    h->Data = NULL;
    h->Hora = NULL;
    h->Duracao = 0;
    return h;
}

void deleteHistorico(Historico *h) {
    if (h == NULL) return;
    g_free(h->Data);
    g_free(h->Hora);
    free(h);
}


// Getters

gint Historico_getId(Historico *h) {
    return h->Id;
}

gint Historico_getUser_Id(Historico *h) {
    return h->User_Id;
}

gint Historico_getMusica_Id(Historico *h) {
    return h->Musica_Id;
}

gchar* Historico_getData(Historico *h) {
    return g_strdup(h->Data);
}

gchar* Historico_getHora(Historico *h) {
    return g_strdup(h->Hora);
}

gint Historico_getDuracao(Historico *h) {
    return h->Duracao;
}


// Setters

void setHistoricoId(Historico *h, gint Id) {
    h->Id = Id;
}

void setHistoricoUser_Id(Historico *h, gint User_Id) {
    h->User_Id = User_Id;
}

void setHistoricoMusica_Id(Historico *h, gint Musica_Id) {
    h->Musica_Id = Musica_Id;
}

void setHistoricoData(Historico *h, gchar *Data) {
    g_free(h->Data);
    h->Data = g_strdup(Data);
}

void setHistoricoHora(Historico *h, gchar *Hora) {
    g_free(h->Hora);
    h->Hora = g_strdup(Hora);
}

void setHistoricoDuracao(Historico *h, gchar *Duracao) {
    gint duration = time_string_to_seconds(Duracao);
    h->Duracao = duration;
}

