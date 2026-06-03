#include "metaindex.h"
#include <stdio.h>

//For exercise 1.3 see man pages for
//int pthread_mutex_lock(pthread_mutex_t *mutex)
//int pthread_mutex_unlock(pthread_mutex_t *mutex);
//int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex);
//int pthread_cond_signal(pthread_cond_t *cond);
//int pthread_cond_broadcast(pthread_cond_t *cond);

void free_filemeta(gpointer data) {
    Filemeta *meta = (Filemeta*)data;
    if (meta->content)
        g_free(meta->content);
    free(meta);
}

Index* index_init(){

	//allocate memory for the index
	Index *index = malloc(sizeof(Index));

	//GHashTable initialization
	index->htable = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, free_filemeta);

	//Useful for exercise 1.3
	//mutex variable initialization
	//int pthread_mutex_init(pthread_mutex_t *mutex,const pthread_mutexattr_t *attr);
	//attr can be used to define non-default attributes (e.g., recursive lock)
	pthread_mutex_init(&index->mutex, NULL);

	//Useful for exercise 1.3
	//condition variable initialization
	//int pthread_cond_init(pthread_cond_t *cond, const pthread_condattr_t *attr);
	//attr can be used to define non-default attributes (e.g., recursive lock)
	pthread_cond_init(&index->cond, NULL);
	return index;

}

//Note: remember that memory allocation and copying must be done here
int index_add(Index *index, char* filename, Filemeta meta){
	pthread_mutex_lock(&index->mutex);
	if (!filename || g_hash_table_contains(index->htable, filename)) {
		pthread_mutex_unlock(&index->mutex);
		return -1;
	}

	char* filename_copy = g_strdup(filename);
	Filemeta* meta_copy = malloc(sizeof(Filemeta));
	if (!meta_copy) {
		g_free(filename_copy);
		pthread_mutex_unlock(&index->mutex);
		return -1;
	}
	meta_copy->size = meta.size;
	meta_copy->refs = meta.refs;

	if (meta.content)
	    meta_copy->content = g_strdup(meta.content);
	else
    meta_copy->content = NULL;

	g_hash_table_insert(index->htable, filename_copy, meta_copy);

	pthread_mutex_unlock(&index->mutex);
	return 0;
}

int index_get(Index *index, char* filename, Filemeta *meta){
	pthread_mutex_lock(&index->mutex);
	if (!filename || !g_hash_table_contains(index->htable, filename)) {
		pthread_mutex_unlock(&index->mutex);
		return -1;
	}

	Filemeta* stored_meta = g_hash_table_lookup(index->htable, filename);
	if (stored_meta) {		
		*meta = *stored_meta;
	} else {
		pthread_mutex_unlock(&index->mutex);
		return -1;
	}

	pthread_mutex_unlock(&index->mutex);
	return 0;
}

int index_increfs(Index *index, char* filename){
    pthread_mutex_lock(&index->mutex);

    if (!filename || !g_hash_table_contains(index->htable, filename)) {
        pthread_mutex_unlock(&index->mutex);
        return -1;
    }

    Filemeta* stored_meta = g_hash_table_lookup(index->htable, filename);
    if (!stored_meta) {
        pthread_mutex_unlock(&index->mutex);
        return -1;
    }

    int refs = ++stored_meta->refs;

    pthread_cond_broadcast(&index->cond);

    pthread_mutex_unlock(&index->mutex);
    return refs;
}

int index_wait_refs(Index* index, char* filename, int nrefs){
    pthread_mutex_lock(&index->mutex);

    if (!filename) {
        pthread_mutex_unlock(&index->mutex);
        return -1;
    }

    Filemeta* stored_meta;

    while (1) {
        stored_meta = g_hash_table_lookup(index->htable, filename);
        if (!stored_meta) {
            pthread_mutex_unlock(&index->mutex);
            return -1;
        }

        if (stored_meta->refs >= nrefs)
            break;

        pthread_cond_wait(&index->cond, &index->mutex);
    }

    pthread_mutex_unlock(&index->mutex);
    return 0;
}

int index_remove(Index* index, char* filename){
	pthread_mutex_lock(&index->mutex);
	if (!filename || !g_hash_table_contains(index->htable, filename)) {
		pthread_mutex_unlock(&index->mutex);
		return -1;
	}

	g_hash_table_remove(index->htable, filename);
	pthread_mutex_unlock(&index->mutex);
	return 0;
}

void index_destroy(Index* index){

	//destroy hashtable
	g_hash_table_destroy(index->htable);

	//Useful for exercice 1.3
	//destroy mutex and cond variables
	pthread_mutex_destroy(&index->mutex);
    pthread_cond_destroy(&index->cond);

	free(index);
}