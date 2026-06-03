#include <signal.h>
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <bpf/libbpf.h>
// #include <errno.h>
#include "systracer.h"
#include "systracer.skel.h"

#define POLL_TIMEOUT_MS 100
#define MAX_SYSCALLS 512

static const char *syscall_table[MAX_SYSCALLS] = {
#include "syscalls_table.h"
};

const char *resolve_syscall(int nr)
{
    if (nr >= 0 && nr < MAX_SYSCALLS && syscall_table[nr])
        return syscall_table[nr];
    return "unknown";
}

static volatile bool exiting = false;

static void sig_handler(int sig){
	exiting = true;
}

int event_handler(void *ctx, void *data, size_t data_sz)
{
	struct data_t *m = data;

	printf("%-6d %-6d %-16s %-6ld %-16s %llu\n", m->pid, m->type, m->command, m->syscall_id, resolve_syscall(m->syscall_id), m->bytes);

	return 0;
}


int main(int argc, char *argv[])
{
   if (argc < 2) {
        printf("usage: ./systracer <pid1> <pid2> ...\n");
        return 1;
    } 

    struct systracer_bpf *skel;
    int err = 0;
    struct ring_buffer *events = NULL;

    signal(SIGINT, sig_handler);

    skel = systracer_bpf__open_and_load();
    if (!skel) {
        fprintf(stderr, "Failed to open BPF object\n");
        return 1;
    }

    /* * CORREÇÃO AQUI:
     * 1. Passamos o objeto do mapa (skel->maps.target_pid_map) diretamente.
     * 2. Temos de passar os tamanhos (sizeof) da chave e do valor.
     */

    for (int i = 1; i < argc; i++) {
        uint32_t pid = atoi(argv[i]);
        uint32_t value = 1;
        bpf_map__update_elem(skel->maps.target_pid_map, &pid, sizeof(pid), &value, sizeof(value), 0);
        printf("Tracing PID %d\n", pid);
    }

    if (err) {
        fprintf(stderr, "Failed to update target_pid_map: %d\n", err);
        goto cleanup;
    }

    err = systracer_bpf__attach(skel);
    if (err) {
        fprintf(stderr, "Failed to attach BPF skeleton: %d\n", err);
        goto cleanup;
    }

	int key = 0;
	unsigned long long lost_counter = 0;
	bpf_map__update_elem(skel->maps.lost_events, &key, sizeof(key), &lost_counter, sizeof(lost_counter), BPF_ANY);
	/* Set up ring buffer polling */
	events = ring_buffer__new(bpf_map__fd(skel->maps.events), event_handler, NULL, NULL);
	if (!events) {
		err = -1;
		printf("Failed to create events ring buffer\n");
		goto cleanup;
	}


	/* Process events */
	printf("%-6s %-6s %-16s %-6s %-16s %s\n", "pid", "layer", "command", "id", "syscall", "bytes");
	while (!exiting) {
		err = ring_buffer__poll(events, POLL_TIMEOUT_MS);
    }

	bpf_map__lookup_elem(skel->maps.lost_events, &key, sizeof(key), &lost_counter, sizeof(lost_counter), BPF_ANY);
	printf("Lost %lld events\n", lost_counter);

	cleanup:
	systracer_bpf__destroy(skel);

	return err < 0 ? -err : 0;
}