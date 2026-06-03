#include "vmlinux.h"
#include <bpf/bpf_helpers.h>
#include <bpf/bpf_tracing.h>
#include <bpf/bpf_core_read.h>
#include "systracer.h"

#define MYOP_OPEN 56
#define MYOP_READ 63
#define MYOP_WRITE 64

struct {
    __uint(type, BPF_MAP_TYPE_RINGBUF);
    __uint(max_entries, 8 * 1024);
} events SEC(".maps");

struct
{
    __uint(type, BPF_MAP_TYPE_ARRAY);
    __type(key, u32);
    __type(value, u64);
    __uint(max_entries, 1);
} lost_events SEC(".maps");


struct {
    __uint(type, BPF_MAP_TYPE_HASH);
    __uint(max_entries, 100);
    __type(key, u32);
    __type(value, u32);
} my_config SEC(".maps");

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
    int8_t rwbs[10]; // Read / Write / Flush / Discard + flags (Sync, Meta, etc.)
    int8_t comm[16];
    int8_t cmd[4];
};

/* ----AUX FUNCS-----  */

/* Function to check if pid in on my_config map or not
 * Returns 1 (to discard) or 0 (to collect)
 */
int to_discard (u32 pid) { // 1 - discard, 0 - collect
    u32 *pid_to_filter = bpf_map_lookup_elem(&my_config, &pid);
    if(pid_to_filter) return 0;
    return 1;
}

/* Function to create and submit an event to the ring buffer
 * Args: u32 pid (process ID), u64 id (syscall ID), int type (0-syscall, 1-VFS, 2-EXT4, 3-BIO)
 * Returns 0 on success.
 */
int create_and_submit_event(u32 pid, u64 id, int type) {

    struct data_t *data = bpf_ringbuf_reserve(&events, sizeof(struct data_t), 0);
    if(!data){
        u32 key=0;
        u64 *lost_counter = bpf_map_lookup_elem(&lost_events, &key);
        if(lost_counter){
            __sync_fetch_and_add(lost_counter, 1);
            return 0;
        }

        return 1;
    }

    data->pid = pid;
    data->syscall_id = id;
    bpf_get_current_comm(&data->command, sizeof(data->command));
    data->type = type;

    bpf_ringbuf_submit(data, 0);
    return 0;
}

/* ----SYSCALL-----  */

SEC("tp/syscalls/sys_enter_openat")
int BPF_PROG(sys_enter_openat, struct pt_regs *regs, long syscall_id, int dfd, const char * filename)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;
    return create_and_submit_event(pid, syscall_id, 0);
}

SEC("tp/syscalls/sys_enter_read")
int BPF_PROG(sys_enter_read, struct pt_regs *regs, long syscall_id, u32 fd)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;
    return create_and_submit_event(pid, syscall_id, 0);
}

SEC("tp/syscalls/sys_enter_write")
int BPF_PROG(sys_enter_write, struct pt_regs *regs, long syscall_id, u32 fd)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;
    return create_and_submit_event(pid, syscall_id, 0);
}


SEC("tp/syscalls/sys_enter_close")
int BPF_PROG(sys_enter_close, struct pt_regs *regs, long syscall_id, u32 fd)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;
    return create_and_submit_event(pid, syscall_id, 0);
}

/* ----VFS-----  */

// int vfs_open(const struct path *path, struct file *file)
SEC("kprobe/vfs_open")
int BPF_KPROBE(vfs_enter_open, const struct path *path, struct file *file)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    return create_and_submit_event(pid, MYOP_OPEN, 1);
}

// ssize_t vfs_read(struct file *, char __user *, size_t, loff_t *);
SEC("kprobe/vfs_read")
int BPF_KPROBE(vfs_read_entry, struct file *file, char *buf, size_t count, loff_t *pos)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    return create_and_submit_event(pid, MYOP_READ, 1);
}

// ssize_t vfs_write(struct file *, const char __user *, size_t, loff_t *)
SEC("kprobe/vfs_write")
int BPF_KPROBE(vfs_enter_write, struct file *file, const char *buf, size_t count, loff_t *pos)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    return create_and_submit_event(pid, MYOP_WRITE, 1);
}

/* ----EXT4-----  */

// int ext4_file_open(struct inode *inode, struct file *filp)
SEC("kprobe/ext4_file_open")
int BPF_KPROBE(ext4_enter_open, struct inode *inode, struct file *filp)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    return create_and_submit_event(pid, MYOP_OPEN, 2);
}

// ssize_t ext4_file_read_iter(struct kiocb *iocb, struct iov_iter *to)
SEC("kprobe/ext4_file_read_iter")
int BPF_KPROBE(ext4_enter_read, struct kiocb *iocb, struct iov_iter *to)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    return create_and_submit_event(pid, MYOP_READ, 2);
}

// ssize_t ext4_file_write_iter(struct kiocb *iocb, struct iov_iter *from)
SEC("kprobe/ext4_file_write_iter")
int BPF_KPROBE(ext4_enter_write, struct inode *inode, struct file *filp)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    return create_and_submit_event(pid, MYOP_WRITE, 2);
}


/* ----BIO-----  */

// Tracepoint: block_rq_insert
// sudo cat /sys/kernel/tracing/events/block/block_rq_issue/format
SEC("tp/block/block_rq_issue")
int trace_block_rq_insertstruct(struct block_rq_submission *ctx)
{
    u32 pid = bpf_get_current_pid_tgid() >> 32;
    if (to_discard(pid)) return 0;

    // Check if this was a read or write based on rwbs flags
    if (ctx->rwbs[0] == 'R') {
        return create_and_submit_event(pid, MYOP_READ, 3);
    } else if (ctx->rwbs[0] == 'W') {
        return create_and_submit_event(pid, MYOP_WRITE, 3);
    }

    return 0;
}

/* ----PROC-----  */

// sudo cat /sys/kernel/tracing/events/sched/sched_process_fork/format
SEC("tp/sched/sched_process_fork")
int BPF_PROG(sys_enter_process_fork, struct pt_regs *regs, long syscall_id, char *parent_comm, pid_t parent_pid, char *child_comm, pid_t child_pid)
{
    int value=1;

    if (to_discard(parent_pid)) return 0;

    bpf_map_update_elem(&my_config, &child_pid, &value, 0);

    return 0;
}


char LICENSE[] SEC("license") = "Dual BSD/GPL";
