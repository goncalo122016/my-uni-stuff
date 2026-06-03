#ifndef SYSTRACER_H
#define SYSTRACER_H

struct data_t {
   int pid;
   char command[16];
   long syscall_id;
   int type; //0-syscall, 1-vfs, 2-ext4, 3-bio
};

#endif
