#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include <bpf/libbpf.h>
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

	char type[10];
	if (m->type == 0) strcpy(type, "syscall");
	else if (m->type == 1) strcpy(type, "VFS");
	else if (m->type == 2) strcpy(type, "EXT4");
	else if (m->type == 3) strcpy(type, "BIO");

	printf("%-6d %-16s %-6ld %-16s %-10s\n", m->pid, m->command, m->syscall_id, resolve_syscall(m->syscall_id), type);

	return 0;
}


int main(int argc, char *argv[])
{
    struct systracer_bpf *skel;
    int err;
	struct ring_buffer *events = NULL;


	/* Cleaner handling of Ctrl-C */
	signal(SIGINT, sig_handler);

	/* Load & verify BPF programs */
	skel = systracer_bpf__open_and_load();
	if (!skel) {
		printf("Failed to open BPF object\n");
		goto cleanup;
	}

	/* Configure a map with the pid to filter */
	if (argc < 2) {
		printf("usage: ./systracer <pid>\n");
	}
	uint32_t key = atoi(argv[1]);
	uint32_t value = 1;
	bpf_map__update_elem(skel->maps.my_config, &key, sizeof(key), &value, sizeof(value), 0);
	printf("Tracing Pid %d\n", key);

	/* Attach hooks */
	err = systracer_bpf__attach(skel);
	if (err) {
		fprintf(stderr, "Failed to attach BPF skeleton: %d\n", err);
		systracer_bpf__destroy(skel);
        goto cleanup;
	}

	key = 0;
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
	printf("%-6s %-16s %-6s %-16s\n", "pid", "command", "id", "syscall");
	while (!exiting) {
		err = ring_buffer__poll(events, POLL_TIMEOUT_MS);
    }

	bpf_map__lookup_elem(skel->maps.lost_events, &key, sizeof(key), &lost_counter, sizeof(lost_counter), BPF_ANY);
	printf("Lost %lld events\n", lost_counter);

	cleanup:
	systracer_bpf__destroy(skel);

	return err < 0 ? -err : 0;
}