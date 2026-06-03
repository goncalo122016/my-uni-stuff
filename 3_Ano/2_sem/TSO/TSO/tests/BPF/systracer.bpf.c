#include "vmlinux.h"
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <bpf/bpf_core_read.h>
#include "systracer.h"

// Números das syscalls (x86_64)
#define SYS_READ    0
#define SYS_WRITE   1
#define SYS_CLOSE   3
#define SYS_OPENAT  257

struct block_rq_submission {
    uint16_t common_type;
    uint8_t common_flags;
    uint8_t common_preempt_count;
    int32_t common_pid;
    uint32_t dev;
    uint64_t sector;
    uint32_t nr_sectors;
    uint32_t nr_bytes;
    uint16_t ioprio;
    int8_t rwbs[10];
    int8_t comm[16];
    int8_t cmd[4];
};


struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 8 * 1024);
} events SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, u32);
    __type(value, u64);
    __uint(max_entries, 1);
} lost_events SEC(".maps");

struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __uint(max_entries, 1); // Apenas precisamos de guardar 1 PID
    __type(key, u32);
    __type(value, u32);
} target_pid_map SEC(".maps");

static __always_inline int to_discard(u32 pid) {
    u32 *val;
    
    val = bpf_map_lookup_elem(&target_pid_map, &pid);
    
    if (val) {
        return 0;
    }
    
    return 1;
}

static __always_inline int create_and_submit_event(u32 pid, u64 id, int layer, unsigned long long bytes) {
    char comm[16];
    bpf_get_current_comm(comm, sizeof(comm));
    if ((__builtin_memcmp(comm, "systracer", 9) == 0) || (__builtin_memcmp(comm, "sudo", 9) == 0) || (__builtin_memcmp(comm, "sshd-session", 9) == 0)) {
        return 0;
    }

    struct data_t *data = bpf_ringbuf_reserve(&events, sizeof(struct data_t), 0);
    if (!data) {
        u32 key = 0;
        u64 *lost_counter = bpf_map_lookup_elem(&lost_events, &key);
        if (lost_counter) {
            __sync_fetch_and_add(lost_counter, 1);
        }
        return 0;
    }

    data->pid = pid;
    data->syscall_id = id;
    data->type = layer;
    data->bytes = bytes;
    bpf_get_current_comm(data->command, sizeof(data->command));
    bpf_ringbuf_submit(data, 0);
    return 0;
}

SEC("raw_tp/sys_enter")
int BPF_PROG(sys_enter, struct pt_regs *regs, long id)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) {
        return 0;
    }    

    // Filtra apenas as syscalls que nos interessam
    if (id != SYS_READ && id != SYS_WRITE &&
        id != SYS_CLOSE && id != SYS_OPENAT) {
        return 0;
    }

    unsigned long long bytes = 0;
    if (id == SYS_READ || id == SYS_WRITE) {
        bytes = PT_REGS_PARM3_CORE(regs);
    }

    return create_and_submit_event(pid, (u64)id, 0, bytes);
}

SEC("kprobe/vfs_read")
int BPF_KPROBE(vfs_enter_read, struct file *file, char *buf, size_t count){
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if(to_discard(pid)) return 0;

    return create_and_submit_event(pid, OP_READ, 1, count);

}

SEC("kprobe/vfs_write")
int BPF_KPROBE(vfs_enter_write, struct file *file, const char *buf, size_t count){
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if(to_discard(pid)) return 0;

    return create_and_submit_event(pid, OP_WRITE, 1, count);

}

SEC("kprobe/ext4_file_read_iter")
int BPF_KPROBE(ext4_enter_read, struct kiocb *iocb, struct iov_iter *to) {
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    unsigned long long bytes = 0;
    bpf_core_read(&bytes, sizeof(bytes), &to->count);

    return create_and_submit_event(pid, OP_READ, 2, bytes);
}

SEC("kprobe/ext4_file_write_iter")
int BPF_KPROBE(ext4_enter_write, struct kiocb *iocb, struct iov_iter *from) {
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    unsigned long long bytes = 0;
    bpf_core_read(&bytes, sizeof(bytes), &from->count);

    return create_and_submit_event(pid, OP_WRITE, 2, bytes);
}
SEC("tp/block/block_rq_issue")
int BPF_PROG(block_issue)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    
    // 1. Filtragem por PID [cite: 66, 67]
    if (to_discard(pid)) 
        return 0;

    /* * 2. Aceder aos dados do tracepoint. 
     * A macro BPF_PROG já definiu 'ctx' como o ponteiro para os argumentos.
     * Castamos o 'ctx' para a estrutura que define o formato do evento[cite: 112].
     */
    struct block_rq_submission *info = (void *)ctx;

    // 3. Distinguir entre leitura e escrita usando o campo rwbs [cite: 127, 128]
    if (info->rwbs[0] == 'R') {
        return create_and_submit_event(pid, OP_READ, 3, info->nr_bytes); // Layer 3 
    } 
    else if (info->rwbs[0] == 'W') {
        return create_and_submit_event(pid, OP_WRITE, 3, info->nr_bytes); // Layer 3 
    }

    return 0;
}

char LICENSE[] SEC("license") = "Dual BSD/GPL";