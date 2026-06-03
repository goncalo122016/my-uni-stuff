#include <stddef.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>
#include <liburing.h>

#define CHECK(cond, fmt, ...)                          \
    do {                                               \
        if (!(cond)) {                                 \
            fprintf(stderr, fmt "\n", ##__VA_ARGS__);  \
            io_uring_queue_exit(&ring);                \
			close(src_fd);                             \
			close(dst_fd);                             \
			return 1;                                  \
        }                                              \
    } while (0)

#define BLK_SIZE 4096
#define DEPTH 32

int main(int argc, char **argv) {
	if (argc != 3) {
		fprintf(stderr, "Usage: %s <source> <destination>\n", argv[0]);
		return 1;
	}

	int src_fd = open(argv[1], O_RDONLY);
	if (src_fd < 0) {
		perror("open source");
		return 1;
	}

	struct stat st;
	if (fstat(src_fd, &st) < 0) {
		perror("fstat");
		close(src_fd);
		return 1;
	}
    assert(st.st_size % BLK_SIZE == 0);

	int dst_fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);
	if (dst_fd < 0) {
		perror("open destination");
		close(src_fd);
		return 1;
	}

	// IO_URING setup
	struct io_uring ring;
	if (io_uring_queue_init(DEPTH, &ring, 0) < 0) {
		perror("io_uring_queue_init");
		close(src_fd);
		close(dst_fd);
		return 1;
	}

	char buffers[DEPTH][BLK_SIZE];
	struct io_uring_sqe *sqe;
	struct io_uring_cqe *cqe;
	int ret;

	for (off_t offset = 0; offset < st.st_size; offset += DEPTH * BLK_SIZE) {
		int batch = DEPTH;

		//READ
		for (int i = 0; i < batch; i++){
			sqe = io_uring_get_sqe(&ring);
			CHECK(sqe != NULL, "io_uring_get_sqe (read) failed");
			io_uring_prep_read(sqe, src_fd, buffers[i], BLK_SIZE, offset + i * BLK_SIZE);
		}
		io_uring_submit_and_wait(&ring, batch);
		int seen = 0;
		while (seen < batch) {
		    struct io_uring_cqe *cqe;
		
		    if (io_uring_peek_cqe(&ring, &cqe) == 0) {
		        CHECK(cqe->res >= 0, "read/write failed");
			
		        io_uring_cqe_seen(&ring, cqe);
		        seen++;
		    }
		}

		// Write
		for (int i = 0; i < batch; i++){
			sqe = io_uring_get_sqe(&ring);
			CHECK(sqe != NULL, "io_uring_get_sqe (read) failed");
			io_uring_prep_write(sqe, dst_fd, buffers[i], BLK_SIZE, offset + i * BLK_SIZE);
		}
		io_uring_submit_and_wait(&ring, batch);
		seen = 0;
		while (seen < batch) {		
		    if (io_uring_peek_cqe(&ring, &cqe) == 0) {
		        CHECK(cqe->res >= 0, "read/write failed");
			
		        io_uring_cqe_seen(&ring, cqe);
		        seen++;
		    }
		}
	}

	io_uring_queue_exit(&ring);
	close(src_fd);
	close(dst_fd);
	return 0;
}
