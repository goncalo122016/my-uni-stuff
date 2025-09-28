#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

#include "../include/utils.h"
#include "../include/Output.h"
#include "../include/entities/Artista.h"

#include "../include/entities/Albums.h"
#include "../include/gestores/gestor_albums.h"


Album *process_Albums(gchar *information, GHashTable *artistas_table) {
    if(artistas_table == NULL || information == NULL){ 
        return NULL;
    }

    information[strcspn(information, "\n")] = 0;

    gchar *clean_information = remove_chars(information, "\"");
    gchar **tokens = g_strsplit(clean_information, ";", 6);
    g_free(clean_information);
    if (tokens == NULL || g_strv_length(tokens) < 5) {  // Garante que há pelo menos 5 campos
        if (tokens) g_strfreev(tokens);  // Libera os tokens, se existirem
        return NULL;
    }

    Album *a = Album_new();
    if (a == NULL) {
        g_strfreev(tokens);
        return NULL;
    }

    // Remove caracteres indesejados e preenche os campos da estrutura
    gint id = g_ascii_strtoll(tokens[0] + 2, NULL, 10);
    setAlbumId(a, id);
    setAlbumTitle(a, tokens[1]);

    gchar *artist_ids = remove_chars(tokens[2], "[]\' ");
    gchar **artist_id_array = g_strsplit(artist_ids, ",", -1);
    g_free(artist_ids);
    gint length = g_strv_length(artist_id_array);
    gint i;
    gint *idsArtists = g_new(gint, length);
    for (i = 0; artist_id_array[i]; i++){
        idsArtists[i] = g_ascii_strtoll(artist_id_array[i] + 1, NULL, 10);
        Artista *art = (Artista *)g_hash_table_lookup(artistas_table, &idsArtists[i]);
        if (!art) {
            deleteAlbum(a);
            g_strfreev(tokens);
            return NULL;
        }
        incrementAlbuns(art);
    }
    setAlbumArtist_Id(a, idsArtists, i);
    setAlbumNumeroArtistas(a, i);
    g_strfreev(artist_id_array);
    g_free(idsArtists);

    // Libera memória temporária
    g_strfreev(tokens);

    return a;
}
