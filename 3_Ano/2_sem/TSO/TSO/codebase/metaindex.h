
#ifndef METAINDEX_H
#define METAINDEX_H

#include <glib.h>
#include <stdint.h>
#include <string.h>
#include <pthread.h>
#include <stdlib.h>
#include <sys/types.h>

#define BLOCK_SIZE 4096 //bytes = 4KB
#define HASH_HEX_LEN 128  // chars vindos do SHA-512
#define N_BLOCKS_ENTRY 8 //número inicial de blocos alocados para cada arquivo, será expandido dinamicamente se necessário

typedef struct block_entry{
	off_t logical_offset;
	char hash[HASH_HEX_LEN + 1];
} BlockEntry;

//struct defining the values of the Hashtable
typedef struct filemeta {
	off_t logical_size;
	size_t nblocks; //number of BlockEntries populadas
	size_t capacity; //number of BlockEntries alocadas
	BlockEntry *blocks;
} Filemeta;



static inline Filemeta *filemeta_create(void) {
	Filemeta *m = malloc(sizeof(Filemeta));
    if (!m) return NULL;
	
	m->logical_size = 0;
	m->nblocks = 0;
	m->capacity = N_BLOCKS_ENTRY;
	m->blocks = (BlockEntry*)malloc(m->capacity * sizeof(BlockEntry));

	return m;
}

static inline void filemeta_free_contents(Filemeta *m) {
    if (m && m->blocks) {
        free(m->blocks);
        m->blocks   = NULL;
        m->nblocks  = 0;
        m->capacity = 0;
    }
}

static inline int filemeta_add_block(Filemeta *m, off_t offset, const char *hash) {
	if (m->nblocks == m->capacity) {
		size_t newcap = m->capacity * 2;
		BlockEntry *nb = (BlockEntry*)realloc(m->blocks, newcap * sizeof(BlockEntry));

		if(nb == NULL) return -1;
		m->blocks = nb;
		m->capacity = newcap;
	}
    m->blocks[m->nblocks].logical_offset = offset;
    strncpy(m->blocks[m->nblocks].hash, hash, HASH_HEX_LEN);
    m->blocks[m->nblocks].hash[HASH_HEX_LEN] = '\0';
    m->nblocks++;
    if (offset + BLOCK_SIZE > m->logical_size)
        m->logical_size = offset + BLOCK_SIZE;
    return 0;
}

static inline const char *filemeta_get_block_hash(const Filemeta *m, off_t offset) {
	for(size_t i = 0; i < m->nblocks; i++) {
		if(m->blocks[i].logical_offset == offset) return m->blocks[i].hash;
	}

	return NULL;
}

static inline int filemeta_copy(Filemeta *dst, const Filemeta *src) {
	dst->logical_size = src->logical_size;
	dst->nblocks = src->nblocks;
	dst->capacity = src->capacity;
	dst->blocks = (BlockEntry *) malloc(dst->capacity * sizeof(BlockEntry));
	if(dst->blocks == NULL) return -1;
	if(src->blocks != NULL) {
		memcpy(dst->blocks, src->blocks, src->nblocks * sizeof(BlockEntry));
	}

	return 0;
}

//structure containing the hashtable structure, global mutex and condition variable
typedef struct index {
	GHashTable *htable;
	GHashTable *refcount_htable;
	pthread_mutex_t mutex;
} Index;

//Initializes the index structure
//Returns NULL in case of failure and a pointer to the struct otherwise
Index* index_init();

//Adds a new key-value entry into the Hashtable
//Returns -1 in case of failure (or if the key already exists) and 0 otherwise
int index_add(Index *index, char* key, Filemeta meta);

//Get the value (meta) for a specific key (key)
//Returns -1 in case of failure (or if the key does not exists) and 0 otherwise
int index_get(Index *index, char* key, Filemeta *meta);


//replaces an existing entry in-place.
//Returns 0 on success, -1 if the key does not exist.
int index_update(Index *index, char *key, Filemeta meta);

//Remove a key-value entry from the Hashtable
//Returns -1 in case of failure (or if the key does not exists) and 0 otherwise
int index_remove(Index* index, char* key);

//Increments the reference count of a block hash
//Returns the new reference count
int index_inc_ref(Index *index, const char *hash);

//Decrements the reference count of a block hash
//Returns the new reference count
int index_dec_ref(Index *index, const char *hash);

//Destroys the index structure
void index_destroy(Index* index);

//Persists the index to a binary file. Returns 0 on success, -1 on error. 
int index_save(Index *index, const char *path);
 
//Loads the index from a binary file. Returns 0 on success, -1 on error. 
int index_load(Index *index, const char *path);

#endif