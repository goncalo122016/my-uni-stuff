
#ifndef PAGECACHE_H
#define PAGECACHE_H

#include <glib.h>
#include <pthread.h>
#include "metaindex.h"

#define CACHE_CAPACITY 1024   /* 1024 × 4 KB = 4 MB */

typedef struct cache_node {
    char hash[HASH_HEX_LEN + 1];
    char data[BLOCK_SIZE];
    struct cache_node *prev;
    struct cache_node *next;
} CacheNode;

typedef struct page_cache {
    GHashTable   *htable;     /* hash string -> CacheNode* (O(1) lookup) */
    CacheNode    *head;       /* most-recently-used end */
    CacheNode    *tail;       /* least-recently-used end */
    size_t        capacity;
    size_t        size;
    pthread_mutex_t mutex;
} PageCache;

/* Allocate a new cache with the given block capacity. */
PageCache *cache_init(size_t capacity);

/* Look up hash in cache.
 * On hit: copy BLOCK_SIZE bytes into buf, return 0.
 * On miss: return -1 (buf unchanged). */
int cache_get(PageCache *cache, const char *hash, char *buf);

/* Insert (or promote) a BLOCK_SIZE entry for hash into the cache.
 * Evicts the LRU entry if the cache is full. */
void cache_put(PageCache *cache, const char *hash, const char *data);

/* Free all resources associated with the cache. */
void cache_destroy(PageCache *cache);

#endif
