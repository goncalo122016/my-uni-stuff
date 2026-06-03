#!/bin/bash

# Exercíco 1
capsh --print
#Current: =
#Bounding set =cap_chown,cap_dac_override,cap_dac_read_search,cap_fowner,cap_fsetid,cap_kill,cap_setgid,cap_setuid,cap_setpcap,cap_linux_immutable,cap_net_bind_service,cap_net_broadcast,cap_net_admin,cap_net_raw,cap_ipc_lock,cap_ipc_owner,cap_sys_module,cap_sys_rawio,cap_sys_chroot,cap_sys_ptrace,cap_sys_pacct,cap_sys_admin,cap_sys_boot,cap_sys_nice,cap_sys_resource,cap_sys_time,cap_sys_tty_config,cap_mknod,cap_lease,cap_audit_write,cap_audit_control,cap_setfcap,cap_mac_override,cap_mac_admin,cap_syslog,cap_wake_alarm,cap_block_suspend,cap_audit_read,cap_perfmon,cap_bpf,cap_checkpoint_restore
#Ambient set =
#Current IAB: 
#Securebits: 00/0x0/1'b0 (no-new-privs=0)
# secure-noroot: no (unlocked)
# secure-no-suid-fixup: no (unlocked)
# secure-keep-caps: no (unlocked)
# secure-no-ambient-raise: no (unlocked)
#uid=1001(goncalo) euid=1001(goncalo)
#gid=1001(goncalo)
#groups=27(sudo),100(users),1001(goncalo)
#Guessed mode: HYBRID (4)

# Exercício 2
gcc webserver.c -o webserver

./webserver 4050
# Success: binded to port 4050

./webserver 80
# Errr on bind: Permission denied

# Dado que a porta 80 é uma porta priveligiada(<1024), a segunda execução é negada de acesso para tal execução. Isto acontece por que por defeito o executável não tem qualquer capability:

getcap ./webserver
#

sudo setcap 'cap_net_bind_service=+ep' ./webserver

getcap ./webserver
#./webserver cap_net_bind_service=ep

./webserver 80
# Success: binded to port 80

# Depois de adicionar a capability "cap_net_bind_service" agora sim o executável já tem permissões para usar a porta pretendida 80, já que esta capability permite o bind a portas <1024. 
