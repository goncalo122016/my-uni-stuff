#ifndef SYSTRACER_H
#define SYSTRACER_H

struct data_t {
   int pid;
   char command[16];
   long syscall_id;
   int type;
   unsigned long long bytes;
};

#endif
