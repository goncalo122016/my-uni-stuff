#include "vmlinux.h"
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <bpf/bpf_core_read.h>
#include "systracer.h"


struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 8 * 1024 * 1024);
} events SEC(".maps");

struct
{
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, u32);
    __type(value, u64);
    __uint(max_entries, 1);
} lost_events SEC(".maps");


SEC("raw_tp/sys_enter")
int syscall_enter_trace(struct bpf_raw_tracepoint_args *ctx)
{
    char comm[16];
    bpf_get_current_comm(comm, sizeof(comm));
    if (__builtin_memcmp(comm, "systracer", 9) == 0)
    {
        return 0;
    }

    struct data_t *data = bpf_ringbuf_reserve(&events, sizeof(struct data_t), 0);
    if(!data) {
        u32 key=0;
        u64 *lost_counter = bpf_map_lookup_elem(&lost_events, &key);
        if(lost_counter){
            __sync_fetch_and_add(lost_counter, 1);
            return 0;
        }
        return 1;
    }

    data->pid = bpf_get_current_pid_tgid() >> 32;
    data->syscall_id = ctx->args[1];
    bpf_get_current_comm(&data->command, sizeof(data->command));

    bpf_ringbuf_submit(data, 0);

   return 0;
}

char LICENSE[] SEC("license") = "Dual BSD/GPL";
