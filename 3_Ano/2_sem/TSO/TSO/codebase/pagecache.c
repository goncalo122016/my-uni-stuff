#include "pagecache.h"
#include <stdlib.h>
#include <string.h>

/* Move node to the head (MRU end) of the LRU list. Caller holds mutex. */
static void move_to_head(PageCache *cache, CacheNode *node) {
    if (node == cache->head)
        return;

    if (node->prev) node->prev->next = node->next;
    if (node->next) node->next->prev = node->prev;
    if (node == cache->tail) cache->tail = node->prev;

    node->prev        = NULL;
    node->next        = cache->head;
    if (cache->head) cache->head->prev = node;
    cache->head = node;
    if (cache->tail == NULL) cache->tail = node;
}

/* Evict the LRU entry. Caller holds mutex. */
static void evict_lru(PageCache *cache) {
    CacheNode *lru = cache->tail;
    if (!lru) return;

    if (lru->prev) lru->prev->next = NULL;
    cache->tail = lru->prev;
    if (cache->head == lru) cache->head = NULL;

    g_hash_table_remove(cache->htable, lru->hash);
    free(lru);
    cache->size--;
}

PageCache *cache_init(size_t capacity) {
    PageCache *cache = malloc(sizeof(PageCache));
    if (!cache) return NULL;

    /* Keys are node->hash (owned by the node); values are the nodes themselves.
     * No GLib-side free functions — we manage memory via the linked list. */
    cache->htable   = g_hash_table_new(g_str_hash, g_str_equal);
    cache->head     = NULL;
    cache->tail     = NULL;
    cache->capacity = capacity;
    cache->size     = 0;
    pthread_mutex_init(&cache->mutex, NULL);
    return cache;
}

int cache_get(PageCache *cache, const char *hash, char *buf) {
    pthread_mutex_lock(&cache->mutex);
    CacheNode *node = g_hash_table_lookup(cache->htable, hash);
    if (!node) {
        pthread_mutex_unlock(&cache->mutex);
        return -1;
    }
    memcpy(buf, node->data, BLOCK_SIZE);
    move_to_head(cache, node);
    pthread_mutex_unlock(&cache->mutex);
    return 0;
}

void cache_put(PageCache *cache, const char *hash, const char *data) {
    pthread_mutex_lock(&cache->mutex);

    CacheNode *existing = g_hash_table_lookup(cache->htable, hash);
    if (existing) {
        move_to_head(cache, existing);
        pthread_mutex_unlock(&cache->mutex);
        return;
    }

    if (cache->size >= cache->capacity)
        evict_lru(cache);

    CacheNode *node = malloc(sizeof(CacheNode));
    if (!node) {
        pthread_mutex_unlock(&cache->mutex);
        return;
    }

    strncpy(node->hash, hash, HASH_HEX_LEN);
    node->hash[HASH_HEX_LEN] = '\0';
    memcpy(node->data, data, BLOCK_SIZE);
    node->prev = NULL;
    node->next = cache->head;
    if (cache->head) cache->head->prev = node;
    cache->head = node;
    if (!cache->tail) cache->tail = node;

    g_hash_table_insert(cache->htable, node->hash, node);
    cache->size++;

    pthread_mutex_unlock(&cache->mutex);
}

void cache_destroy(PageCache *cache) {
    if (!cache) return;
    pthread_mutex_lock(&cache->mutex);

    CacheNode *cur = cache->head;
    while (cur) {
        CacheNode *next = cur->next;
        free(cur);
        cur = next;
    }
    g_hash_table_destroy(cache->htable);

    pthread_mutex_unlock(&cache->mutex);
    pthread_mutex_destroy(&cache->mutex);
    free(cache);
}
