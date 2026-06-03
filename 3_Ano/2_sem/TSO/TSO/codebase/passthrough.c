/*
  FUSE: Filesystem in Userspace
  Copyright (C) 2001-2007  Miklos Szeredi <miklos@szeredi.hu>
  Copyright (C) 2011       Sebastian Pipping <sebastian@pipping.org>

  This program can be distributed under the terms of the GNU GPLv2.
  See the file COPYING.
*/

/** @file
 *
 * This file system mirrors the existing file system hierarchy of the
 * system, starting at the root file system. This is implemented by
 * just "passing through" all requests to the corresponding user-space
 * libc functions. Its performance is terrible.
 *
 * Compile with
 *
 *     gcc -Wall passthrough.c `pkg-config fuse3 --cflags --libs` -o passthrough
 *
 * ## Source code ##
 * \include passthrough.c
 */

#define FUSE_USE_VERSION 31

#define _GNU_SOURCE

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif

#include <stdlib.h>
#include <pthread.h>
#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#ifdef __FreeBSD__
#include <sys/socket.h>
#include <sys/un.h>
#endif
#include <sys/time.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include "passthrough_helpers.h"
#include "metaindex.h"
#include "pagecache.h"

#define DEBUG "/tmp/debug.log"
#define BACKEND_PATH "/backend"
#define BLOCKS_DIR BACKEND_PATH "/.dedupblocks"
#define INDEX_PATH BACKEND_PATH "/.dedupindex.bin"

#ifdef DEBUG
typedef struct context
{
	uint64_t create;
	uint64_t open;
	uint64_t close;
	uint64_t write;
	uint64_t read;
	uint64_t getattr;
	uint64_t dedup_hits;
	pthread_mutex_t mutex;
	FILE *fp;
	Index *index;
	PageCache *cache;
} Context;
#endif

static void get_full_path(char fpath[PATH_MAX], const char *path) {
    if (strcmp(path, "/") == 0) {
        snprintf(fpath, PATH_MAX, "%s", BACKEND_PATH);
    } else {
        snprintf(fpath, PATH_MAX, "%s%s", BACKEND_PATH, path);
    }
}

static int fill_dir_plus = 0;

static void *xmp_init(struct fuse_conn_info *conn,
					  struct fuse_config *cfg)
{
	(void)conn;
	cfg->use_ino = 1;

	/* Pick up changes from lower filesystem right away. This is
	   also necessary for better hardlink support. When the kernel
	   calls the unlink() handler, it does not know the inode of
	   the to-be-removed entry and can therefore not invalidate
	   the cache of the associated inode - resulting in an
	   incorrect st_nlink value being reported for any remaining
	   hardlinks to this inode. */
	cfg->entry_timeout = 0;
	cfg->attr_timeout = 0;
	cfg->negative_timeout = 0;

	// Initialize context
	Context *ctx = malloc(sizeof(Context));
	pthread_mutex_init(&ctx->mutex, NULL);

	// Create hidden block store directory if it doesn't exist yet
	mkdir(BLOCKS_DIR, 0700);

	// Initialize metaindex and load persisted index if present
	ctx->index = index_init();
	if (access(INDEX_PATH, F_OK) == 0)
		index_load(ctx->index, INDEX_PATH);

	ctx->cache = cache_init(CACHE_CAPACITY);

#ifdef DEBUG
	ctx->open = 0;
	ctx->close = 0;
	ctx->read = 0;
	ctx->write = 0;
	ctx->dedup_hits = 0;
	struct fuse_context *f_ctx = fuse_get_context();
	ctx->fp = fopen(DEBUG, "w");
	printf("[Thread %d] Init called, userid %d, pid %d\n", gettid(), f_ctx->uid, f_ctx->pid);
	fprintf(ctx->fp, "[Thread %d] Init called, userid %d, pid %d\n", gettid(), f_ctx->uid, f_ctx->pid);
#endif

	return ctx;
}

static void xmp_destroy(void *private_data)
{

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = (Context *)private_data;
	pthread_mutex_destroy(&p_ctx->mutex);
	cache_destroy(p_ctx->cache);
	index_save(p_ctx->index, INDEX_PATH);
	index_destroy(p_ctx->index);

#ifdef DEBUG
	printf("[Thread %d] Destroy called, userid %d, pid %d\n", gettid(), f_ctx->uid, f_ctx->pid);
	printf("[Thread %d] Open() - %lu, Read() - %lu, Write() - %lu, Close() - %lu, DedupHits() - %lu\n", gettid(), p_ctx->open, p_ctx->read, p_ctx->write, p_ctx->close, p_ctx->dedup_hits);
	fprintf(p_ctx->fp, "[Thread %d] Destroy called, userid %d, pid %d\n", gettid(), f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Open() - %lu, Read() - %lu, Write() - %lu, Close() - %lu, DedupHits() - %lu\n", gettid(), p_ctx->open, p_ctx->read, p_ctx->write, p_ctx->close, p_ctx->dedup_hits);
	fclose(p_ctx->fp);
#endif

	free(private_data);
}

static int xmp_getattr(const char *path, struct stat *stbuf,
					   struct fuse_file_info *fi)
{
	(void)fi;
	int res;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = (Context *)f_ctx->private_data;


	#ifdef DEBUG
	p_ctx->getattr++;
	printf("[Thread %d] Getattr for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Getattr for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	#endif

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = lstat(fpath, stbuf);

	if (res == -1)
		return -errno;

	// For regular files, report logical size from the index
	if (S_ISREG(stbuf->st_mode)) {
		Filemeta meta;
		if (index_get(p_ctx->index, (char *)path, &meta) == 0) {
			stbuf->st_size   = meta.logical_size;
			stbuf->st_blocks = (meta.logical_size + 511) / 512;
			filemeta_free_contents(&meta);
		}
	}

	return 0;
}

static int xmp_access(const char *path, int mask)
{
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = access(fpath, mask);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_readlink(const char *path, char *buf, size_t size)
{
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = readlink(fpath, buf, size - 1);
	if (res == -1)
		return -errno;

	buf[res] = '\0';
	return 0;
}

static int xmp_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
					   off_t offset, struct fuse_file_info *fi,
					   enum fuse_readdir_flags flags)
{
	DIR *dp;
	struct dirent *de;

	(void)offset;
	(void)fi;
	(void)flags;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	dp = opendir(fpath);
	if (dp == NULL)
		return -errno;

	while ((de = readdir(dp)) != NULL)
	{
		// Hide internal dedup storage from the user
		if (strcmp(de->d_name, ".dedupblocks") == 0 ||
		    strcmp(de->d_name, ".dedupindex.bin") == 0)
			continue;

		struct stat st;
		memset(&st, 0, sizeof(st));
		st.st_ino = de->d_ino;
		st.st_mode = de->d_type << 12;
		if (filler(buf, de->d_name, &st, 0, fill_dir_plus))
			break;
	}

	closedir(dp);
	return 0;
}

static int xmp_mknod(const char *path, mode_t mode, dev_t rdev)
{
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = mknod_wrapper(AT_FDCWD, fpath, NULL, mode, rdev);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_mkdir(const char *path, mode_t mode)
{
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = mkdir(fpath, mode);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_rmdir(const char *path)
{
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = rmdir(fpath);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_symlink(const char *from, const char *to)
{
	int res;

	char fto[PATH_MAX];
	get_full_path(fto, to);
	res = symlink(from, fto);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_unlink(const char *path)
{
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = unlink(fpath);
	if (res == -1)
		return -errno;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = f_ctx->private_data;
	
	Filemeta meta;
	if (index_get(p_ctx->index, (char *)path, &meta) == 0) {
		for (size_t i = 0; i < meta.nblocks; i++) {
			int ref_count = index_dec_ref(p_ctx->index, meta.blocks[i].hash);
			if (ref_count == 0) {
				char bpath[PATH_MAX];
				snprintf(bpath, sizeof(bpath), "%s/%s", BLOCKS_DIR, meta.blocks[i].hash);
				unlink(bpath);
			}
		}
		filemeta_free_contents(&meta);
	}
	index_remove(p_ctx->index, (char *)path);

	return 0;
}

static int xmp_rename(const char *from, const char *to, unsigned int flags)
{
	int res;

	if (flags)
		return -EINVAL;

	char ffrom[PATH_MAX], fto[PATH_MAX];
	get_full_path(ffrom, from);
	get_full_path(fto, to);
	res = rename(ffrom, fto);
	if (res == -1)
		return -errno;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = f_ctx->private_data;
	Filemeta meta;
	if (index_get(p_ctx->index, (char *)from, &meta) == 0) {
		index_remove(p_ctx->index, (char *)from);
		if (index_add(p_ctx->index, (char *)to, meta) != 0)
			index_update(p_ctx->index, (char *)to, meta);
		filemeta_free_contents(&meta);
	}

	return 0;
}

static int xmp_link(const char *from, const char *to)
{
	int res;

	char ffrom[PATH_MAX], fto[PATH_MAX];
	get_full_path(ffrom, from);
	get_full_path(fto, to);
	res = link(ffrom, fto);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_chmod(const char *path, mode_t mode,
					 struct fuse_file_info *fi)
{
	(void)fi;
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = chmod(fpath, mode);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_chown(const char *path, uid_t uid, gid_t gid,
					 struct fuse_file_info *fi)
{
	(void)fi;
	int res;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = lchown(fpath, uid, gid);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_truncate(const char *path, off_t size,
						struct fuse_file_info *fi)
{
	int res;

	if (fi != NULL)
		res = ftruncate(fi->fh, size);
	else {
		char fpath[PATH_MAX];
		get_full_path(fpath, path);
		res = truncate(fpath, size);
	}
	if (res == -1)
		return -errno;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = f_ctx->private_data;
	
	Filemeta meta;
	if (index_get(p_ctx->index, (char *)path, &meta) == 0) {
		if (size < meta.logical_size) {
			size_t keep_blocks = 0;
			for (size_t i = 0; i < meta.nblocks; i++) {
				if (meta.blocks[i].logical_offset < size) {
					meta.blocks[keep_blocks] = meta.blocks[i];
					keep_blocks++;
				} else {
					int ref_count = index_dec_ref(p_ctx->index, meta.blocks[i].hash);
					if (ref_count == 0) {
						char bpath[PATH_MAX];
						snprintf(bpath, sizeof(bpath), "%s/%s", BLOCKS_DIR, meta.blocks[i].hash);
						unlink(bpath);
					}
				}
			}
			meta.nblocks = keep_blocks;
			meta.logical_size = size;
			index_update(p_ctx->index, (char *)path, meta);
		}
		filemeta_free_contents(&meta);
	}

	return 0;
}

#ifdef HAVE_UTIMENSAT
static int xmp_utimens(const char *path, const struct timespec ts[2],
					   struct fuse_file_info *fi)
{
	(void)fi;
	int res;

	/* don't use utime/utimes since they follow symlinks */
	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = utimensat(0, fpath, ts, AT_SYMLINK_NOFOLLOW);
	if (res == -1)
		return -errno;

	return 0;
}
#endif

static int xmp_create(const char *path, mode_t mode,
					  struct fuse_file_info *fi)
{
	int res;

#ifdef DEBUG
	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = f_ctx->private_data;
	pthread_mutex_lock(&p_ctx->mutex);
	p_ctx->create++;
	printf("[Thread %d] Create for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Create for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	pthread_mutex_unlock(&p_ctx->mutex);
#endif

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = open(fpath, fi->flags, mode);
	if (res == -1)
		return -errno;

	fi->fh = res;

	// Register an empty metadata entry so getattr returns size=0 immediately
	struct fuse_context *f_ctx_create = fuse_get_context();
	Context *p_ctx_create = f_ctx_create->private_data;
	Filemeta *empty_meta = filemeta_create();
	index_add(p_ctx_create->index, (char *)path, *empty_meta);
	filemeta_free_contents(empty_meta);
	free(empty_meta);

	return 0;
}

static int xmp_open(const char *path, struct fuse_file_info *fi)
{
	int res;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = (Context *)f_ctx->private_data;

	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = open(fpath, fi->flags);

#ifdef DEBUG
	p_ctx->open++;
	printf("[Thread %d] Open for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Open for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
#endif

	if (res == -1)
		return -errno;

	fi->fh = res;
	return 0;
}

static int xmp_read(const char *path, char *buf, size_t size, off_t offset,
					struct fuse_file_info *fi)
{
	int res;

#ifdef DEBUG
	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = f_ctx->private_data;
	pthread_mutex_lock(&p_ctx->mutex);
	p_ctx->read++;
	printf("[Thread %d] Read for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Read for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	pthread_mutex_unlock(&p_ctx->mutex);
#else
	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = (Context *)f_ctx->private_data;
#endif

	(void)fi;

	// Look up this file's block map in the index
	Filemeta meta;
	if (index_get(p_ctx->index, (char *)path, &meta) != 0)
		return 0; // file not in index yet (empty file)

	// Clamp read to logical file size
	if (offset >= meta.logical_size) {
		filemeta_free_contents(&meta);
		return 0;
	}
	if (offset + (off_t)size > meta.logical_size)
		size = (size_t)(meta.logical_size - offset);

	size_t total_read = 0;
	while (total_read < size) {
		// Which block covers the current position?
		off_t cur_pos   = offset + (off_t)total_read;
		off_t block_off = (cur_pos / BLOCK_SIZE) * BLOCK_SIZE;
		size_t buf_off  = (size_t)(cur_pos - block_off);   // byte within the block
		size_t to_copy  = BLOCK_SIZE - buf_off;
		if (total_read + to_copy > size)
			to_copy = size - total_read;

		const char *hash = filemeta_get_block_hash(&meta, block_off);
		if (hash == NULL) {
			// Sparse — return zeros
			memset(buf + total_read, 0, to_copy);
		} else {
			char block_data[BLOCK_SIZE];
			if (cache_get(p_ctx->cache, hash, block_data) != 0) {
				// Cache miss: fetch full block from disk and populate cache
				char bpath[PATH_MAX];
				snprintf(bpath, sizeof(bpath), "%s/%s", BLOCKS_DIR, hash);
				int bfd = open(bpath, O_RDONLY);
				if (bfd == -1) {
					filemeta_free_contents(&meta);
					return -errno;
				}
				ssize_t rd = pread(bfd, block_data, BLOCK_SIZE, 0);
				close(bfd);
				if (rd < 0) {
					filemeta_free_contents(&meta);
					return -errno;
				}
				cache_put(p_ctx->cache, hash, block_data);
			}
			memcpy(buf + total_read, block_data + buf_off, to_copy);
		}
		total_read += to_copy;
	}

	filemeta_free_contents(&meta);
	res = (int)total_read;
	return res;
}

static int xmp_write(const char *path, const char *buf, size_t size,
					 off_t offset, struct fuse_file_info *fi)
{
	(void)fi;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = (Context *)f_ctx->private_data;

#ifdef DEBUG
	pthread_mutex_lock(&p_ctx->mutex);
	p_ctx->write++;
	printf("[Thread %d] Write for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Write for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	pthread_mutex_unlock(&p_ctx->mutex);
#endif

	// Load (or create) Filemeta for this file
	Filemeta meta;
	int meta_exists = (index_get(p_ctx->index, (char *)path, &meta) == 0);
	if (!meta_exists) {
		Filemeta *nm = filemeta_create();
		meta = *nm;
		free(nm);
	}

	size_t nblocks       = size / BLOCK_SIZE;
	size_t total_written = 0;

	for (size_t i = 0; i < nblocks; i++) {
		const char *block_buf = buf + i * BLOCK_SIZE;
		off_t       block_off = offset + (off_t)(i * BLOCK_SIZE);

		// 1. Hash the 4KB block
		char hash[HASH_HEX_LEN + 1];
		if (sha512_hex(block_buf, BLOCK_SIZE, hash) != 0) {
			filemeta_free_contents(&meta);
			return -EIO;
		}

		// 2. Increment reference count for the hash
		int ref_count = index_inc_ref(p_ctx->index, hash);

		char bpath[PATH_MAX];
		snprintf(bpath, sizeof(bpath), "%s/%s", BLOCKS_DIR, hash);

		if (ref_count == 1) {
			// New unique block
			int bfd = open(bpath, O_CREAT | O_EXCL | O_WRONLY, 0600);
			if (bfd == -1) {
				if (errno != EEXIST) {
					index_dec_ref(p_ctx->index, hash);
					filemeta_free_contents(&meta);
					return -errno;
				}
				// EEXIST: race with another thread, block already written — OK
				pthread_mutex_lock(&p_ctx->mutex);
				p_ctx->dedup_hits++;
				pthread_mutex_unlock(&p_ctx->mutex);
			} else {
				ssize_t wr = write(bfd, block_buf, BLOCK_SIZE);
				close(bfd);
				if (wr != (ssize_t)BLOCK_SIZE) {
					index_dec_ref(p_ctx->index, hash);
					unlink(bpath);
					filemeta_free_contents(&meta);
					return -EIO;
				}
				// Warm the cache so a subsequent read avoids a disk round-trip
				cache_put(p_ctx->cache, hash, block_buf);
			}
		} else {
			// Duplicate block — skip the write entirely
			pthread_mutex_lock(&p_ctx->mutex);
			p_ctx->dedup_hits++;
			pthread_mutex_unlock(&p_ctx->mutex);
		}

		// 3. Record logical_offset -> hash mapping
		if (filemeta_add_block(&meta, block_off, hash) != 0) {
			index_dec_ref(p_ctx->index, hash);
			if (ref_count == 1) unlink(bpath);
			filemeta_free_contents(&meta);
			return -ENOMEM;
		}

		total_written += BLOCK_SIZE;
	}

	// 4. Persist updated metadata in the index
	if (meta_exists)
		index_update(p_ctx->index, (char *)path, meta);
	else {
		if (index_add(p_ctx->index, (char *)path, meta) != 0)
			index_update(p_ctx->index, (char *)path, meta);
	}
	filemeta_free_contents(&meta);

	return (int)total_written;
}

static int xmp_statfs(const char *path, struct statvfs *stbuf)
{
	int res;
	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	res = statvfs(fpath, stbuf);
	if (res == -1)
		return -errno;

	return 0;
}

static int xmp_release(const char *path, struct fuse_file_info *fi)
{
	(void)path;

	struct fuse_context *f_ctx = fuse_get_context();
	Context *p_ctx = f_ctx->private_data;

	#ifdef DEBUG
	p_ctx->close++;
	printf("[Thread %d] Close for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	fprintf(p_ctx->fp, "[Thread %d] Close for path %s, userid %d, pid %d\n", gettid(), path, f_ctx->uid, f_ctx->pid);
	#endif

	int res = close(fi->fh);

	return res;
}

static int xmp_fsync(const char *path, int isdatasync,
					 struct fuse_file_info *fi)
{

	(void)path;

	// If the datasync parameter is non-zero, then only the user data should be flushed, not the meta data.
	if (isdatasync == 0)
	{
		return fsync(fi->fh);
	}

	return fdatasync(fi->fh);
}

#ifdef HAVE_POSIX_FALLOCATE
static int xmp_fallocate(const char *path, int mode,
						 off_t offset, off_t length, struct fuse_file_info *fi)
{
	int fd;
	int res;

	(void)fi;

	if (mode)
		return -EOPNOTSUPP;

	if (fi == NULL) {
		char fpath[PATH_MAX];
		get_full_path(fpath, path);
		fd = open(fpath, O_WRONLY);
	} else
		fd = fi->fh;

	if (fd == -1)
		return -errno;

	res = -posix_fallocate(fd, offset, length);

	if (fi == NULL)
		close(fd);
	return res;
}
#endif

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
static int xmp_setxattr(const char *path, const char *name, const char *value,
						size_t size, int flags)
{
	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	int res = lsetxattr(fpath, name, value, size, flags);
	if (res == -1)
		return -errno;
	return 0;
}

static int xmp_getxattr(const char *path, const char *name, char *value,
						size_t size)
{
	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	int res = lgetxattr(fpath, name, value, size);
	if (res == -1)
		return -errno;
	return res;
}

static int xmp_listxattr(const char *path, char *list, size_t size)
{
	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	int res = llistxattr(fpath, list, size);
	if (res == -1)
		return -errno;
	return res;
}

static int xmp_removexattr(const char *path, const char *name)
{
	char fpath[PATH_MAX];
	get_full_path(fpath, path);
	int res = lremovexattr(fpath, name);
	if (res == -1)
		return -errno;
	return 0;
}
#endif /* HAVE_SETXATTR */

#ifdef HAVE_COPY_FILE_RANGE
static ssize_t xmp_copy_file_range(const char *path_in,
								   struct fuse_file_info *fi_in,
								   off_t offset_in, const char *path_out,
								   struct fuse_file_info *fi_out,
								   off_t offset_out, size_t len, int flags)
{
	int fd_in, fd_out;
	ssize_t res;

	if (fi_in == NULL) {
		char fpath_in[PATH_MAX];
		get_full_path(fpath_in, path_in);
		fd_in = open(fpath_in, O_RDONLY);
	} else
		fd_in = fi_in->fh;

	if (fd_in == -1)
		return -errno;

	if (fi_out == NULL) {
		char fpath_out[PATH_MAX];
		get_full_path(fpath_out, path_out);
		fd_out = open(fpath_out, O_WRONLY);
	} else
		fd_out = fi_out->fh;

	if (fd_out == -1)
	{
		close(fd_in);
		return -errno;
	}

	res = copy_file_range(fd_in, &offset_in, fd_out, &offset_out, len,
						  flags);
	if (res == -1)
		res = -errno;

	if (fi_out == NULL)
		close(fd_out);
	if (fi_in == NULL)
		close(fd_in);

	return res;
}
#endif

static off_t xmp_lseek(const char *path, off_t off, int whence, struct fuse_file_info *fi)
{
	int fd;
	off_t res;

	if (fi == NULL) {
		char fpath[PATH_MAX];
		get_full_path(fpath, path);
		fd = open(fpath, O_RDONLY);
	} else
		fd = fi->fh;

	if (fd == -1)
		return -errno;

	res = lseek(fd, off, whence);
	if (res == -1)
		res = -errno;

	if (fi == NULL)
		close(fd);
	return res;
}

static const struct fuse_operations xmp_oper = {
	.init = xmp_init,
	.destroy = xmp_destroy,
	.getattr = xmp_getattr,
	.access = xmp_access,
	.readlink = xmp_readlink,
	.readdir = xmp_readdir,
	.mknod = xmp_mknod,
	.mkdir = xmp_mkdir,
	.symlink = xmp_symlink,
	.unlink = xmp_unlink,
	.rmdir = xmp_rmdir,
	.rename = xmp_rename,
	.link = xmp_link,
	.chmod = xmp_chmod,
	.chown = xmp_chown,
	.truncate = xmp_truncate,
#ifdef HAVE_UTIMENSAT
	.utimens = xmp_utimens,
#endif
	.open = xmp_open,
	.create = xmp_create,
	.read = xmp_read,
	.write = xmp_write,
	.statfs = xmp_statfs,
	.release = xmp_release,
	.fsync = xmp_fsync,
#ifdef HAVE_POSIX_FALLOCATE
	.fallocate = xmp_fallocate,
#endif
#ifdef HAVE_SETXATTR
	.setxattr = xmp_setxattr,
	.getxattr = xmp_getxattr,
	.listxattr = xmp_listxattr,
	.removexattr = xmp_removexattr,
#endif
#ifdef HAVE_COPY_FILE_RANGE
	.copy_file_range = xmp_copy_file_range,
#endif
	.lseek = xmp_lseek,
};

int main(int argc, char *argv[])
{
	enum
	{
		MAX_ARGS = 10
	};
	int i, new_argc;
	char *new_argv[MAX_ARGS];

	umask(0);
	/* Process the "--plus" option apart */
	for (i = 0, new_argc = 0; (i < argc) && (new_argc < MAX_ARGS); i++)
	{
		if (!strcmp(argv[i], "--plus"))
		{
			fill_dir_plus = FUSE_FILL_DIR_PLUS;
		}
		else
		{
			new_argv[new_argc++] = argv[i];
		}
	}

	return fuse_main(new_argc, new_argv, &xmp_oper, NULL);
}