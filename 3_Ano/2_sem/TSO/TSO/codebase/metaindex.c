#include "metaindex.h"
#include <stdio.h>

void filemeta_gfree(gpointer data) {
	Filemeta *m = (Filemeta *)data;
	filemeta_free_contents(m);
	free(m);
}

Index* index_init(){

	//allocate memory for the index
	Index *index = malloc(sizeof(Index));
	if(index == NULL) return NULL;

	//GHashTable initialization
	index->htable = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, filemeta_gfree);
	index->refcount_htable = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_free);

	//mutex variable initialization
	//int pthread_mutex_init(pthread_mutex_t *mutex,const pthread_mutexattr_t *attr);
	//attr can be used to define non-default attributes (e.g., recursive lock)
	pthread_mutex_init(&index->mutex, NULL);

	return index;

}

int index_add(Index *index, char *key, Filemeta meta) {
    int res = -1;
 
    char *nkey = strdup(key);
    if (!nkey) return -1;
 
    Filemeta *value = malloc(sizeof(Filemeta));
    if (!value) { free(nkey); return -1; }
 
    if (filemeta_copy(value, &meta) != 0) {
        free(value);
        free(nkey);
        return -1;
    }
 
    pthread_mutex_lock(&index->mutex);
    if (g_hash_table_insert(index->htable, nkey, value) == TRUE)
        res = 0;
    else {
        // a chave ja existia 
        res = -1;
    }
    pthread_mutex_unlock(&index->mutex);
 
    return res;
}
 
int index_get(Index *index, char *key, Filemeta *meta) {
    int res = -1;
 
    pthread_mutex_lock(&index->mutex);
    Filemeta *value = (Filemeta *)g_hash_table_lookup(index->htable, key);
    if (value != NULL) {
        if (filemeta_copy(meta, value) == 0)
            res = 0;
    }
    pthread_mutex_unlock(&index->mutex);
 
    return res;
}

//...
int index_update(Index *index, char *key, Filemeta meta) {
	int res = -1;

	Filemeta *value = malloc(sizeof(Filemeta));
	if(value == NULL) return -1;

	if(filemeta_copy(value, &meta) != 0){
		free(value);
		return -1;
	}

	pthread_mutex_lock(&index->mutex);
	Filemeta *existing = (Filemeta *)g_hash_table_lookup(index->htable, key);
	if(existing != NULL) {
		char *nkey = strdup(key);
		if(nkey != NULL) {
			g_hash_table_replace(index->htable, nkey, value);
			res = 0;
		} else {
			filemeta_gfree(value);
		}
	} else {
		filemeta_gfree(value);
	}
	pthread_mutex_unlock(&index->mutex);

	return res;
}


int index_remove(Index *index, char *key) {
    int res = -1;
 
    pthread_mutex_lock(&index->mutex);
    if (g_hash_table_lookup(index->htable, key) != NULL) {
        if (g_hash_table_remove(index->htable, key) == TRUE)
            res = 0;
    }
    pthread_mutex_unlock(&index->mutex);
 
    return res;
}
 

int index_inc_ref(Index *index, const char *hash) {
    uint32_t *count;
    pthread_mutex_lock(&index->mutex);
    count = (uint32_t*)g_hash_table_lookup(index->refcount_htable, hash);
    if (count) {
        (*count)++;
    } else {
        count = malloc(sizeof(uint32_t));
        *count = 1;
        g_hash_table_insert(index->refcount_htable, strdup(hash), count);
    }
    int res = *count;
    pthread_mutex_unlock(&index->mutex);
    return res;
}

int index_dec_ref(Index *index, const char *hash) {
    uint32_t *count;
    int res = -1;
    pthread_mutex_lock(&index->mutex);
    count = (uint32_t*)g_hash_table_lookup(index->refcount_htable, hash);
    if (count) {
        if (*count > 0) (*count)--;
        res = *count;
        if (*count == 0) {
            g_hash_table_remove(index->refcount_htable, hash);
        }
    }
    pthread_mutex_unlock(&index->mutex);
    return res;
}

void index_destroy(Index* index){

	//destroy hashtable
	g_hash_table_destroy(index->htable);
	g_hash_table_destroy(index->refcount_htable);

	//destroy mutex and cond variables
	pthread_mutex_destroy(&index->mutex);

	free(index);
}

/*
 * Binary format per file entry:
 *   [uint32_t  key_len]
 *   [char      key[key_len]]        (no null terminator on disk)
 *   [off_t     logical_size]
 *   [uint64_t  nblocks]
 *   for each block:
 *     [off_t   logical_offset]
 *     [char    hash[HASH_HEX_LEN+1]]
 */
int index_save(Index *index, const char *path) {
    FILE *f = fopen(path, "wb");
    if (!f) return -1;
 
    pthread_mutex_lock(&index->mutex);
 
    GHashTableIter iter;
    gpointer k, v;
    g_hash_table_iter_init(&iter, index->htable);
 
    int res = 0;
    while (g_hash_table_iter_next(&iter, &k, &v)) {
        char *key       = (char *)k;
        Filemeta *meta  = (Filemeta *)v;
 
        uint32_t key_len = (uint32_t)strlen(key);
        if (fwrite(&key_len, sizeof(key_len), 1, f) != 1) { res = -1; break; }
        if (fwrite(key, 1, key_len, f) != key_len)        { res = -1; break; }
 
        if (fwrite(&meta->logical_size, sizeof(meta->logical_size), 1, f) != 1) { res = -1; break; }
 
        uint64_t nb = (uint64_t)meta->nblocks;
        if (fwrite(&nb, sizeof(nb), 1, f) != 1) { res = -1; break; }
 
        for (size_t i = 0; i < meta->nblocks; i++) {
            if (fwrite(&meta->blocks[i].logical_offset, sizeof(off_t), 1, f) != 1) { res = -1; break; }
            if (fwrite(meta->blocks[i].hash, 1, HASH_HEX_LEN + 1, f) != (size_t)(HASH_HEX_LEN + 1)) { res = -1; break; }
        }
        if (res == -1) break;
    }
 
    pthread_mutex_unlock(&index->mutex);
    fclose(f);
    return res;
}
 
int index_load(Index *index, const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return -1;  // no saved index yet is fine — caller should check errno
 
    int res = 0;
    while (!feof(f)) {
        uint32_t key_len;
        if (fread(&key_len, sizeof(key_len), 1, f) != 1) {
            if (feof(f)) break;  // clean end-of-file
            res = -1; break;
        }
 
        char *key = malloc(key_len + 1);
        if (!key) { res = -1; break; }
        if (fread(key, 1, key_len, f) != key_len) { free(key); res = -1; break; }
        key[key_len] = '\0';
 
        Filemeta* meta = filemeta_create();
 
        if (fread(&meta->logical_size, sizeof(meta->logical_size), 1, f) != 1) {
            free(key); filemeta_free_contents(meta); res = -1; break;
        }
 
        uint64_t nb;
        if (fread(&nb, sizeof(nb), 1, f) != 1) {
            free(key); filemeta_free_contents(meta); res = -1; break;
        }
 
        for (uint64_t i = 0; i < nb; i++) {
            off_t off;
            char hash[HASH_HEX_LEN + 1];
            if (fread(&off, sizeof(off_t), 1, f) != 1) { res = -1; break; }
            if (fread(hash, 1, HASH_HEX_LEN + 1, f) != (size_t)(HASH_HEX_LEN + 1)) { res = -1; break; }
            if (filemeta_add_block(meta, off, hash) != 0) { res = -1; break; }
            index_inc_ref(index, hash);
        }
 
        if (res == -1) { free(key); filemeta_free_contents(meta); break; }
 
        // insert into htable directly (bypasses index_add's copy — we own meta already)
        Filemeta *heap_meta = malloc(sizeof(Filemeta));
        if (!heap_meta) { free(key); filemeta_free_contents(meta); res = -1; break; }
        *heap_meta = *meta;  // transfer ownership of blocks pointer
 
        pthread_mutex_lock(&index->mutex);
        g_hash_table_insert(index->htable, key, heap_meta);  // htable takes ownership of key
        pthread_mutex_unlock(&index->mutex);
    }
 
    fclose(f);
    return res;
}