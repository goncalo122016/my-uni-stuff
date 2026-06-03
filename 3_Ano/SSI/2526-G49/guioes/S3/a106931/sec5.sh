#!/bin/bash

# Exercício 1
capsh --print
# Current: =
# Bounding set =cap_chown,cap_dac_override,cap_dac_read_search,cap_fowner,
#   cap_fsetid,cap_kill,cap_setgid,cap_setuid,cap_setpcap,cap_linux_immutable,
#   cap_net_bind_service,cap_net_broadcast,cap_net_admin,cap_net_raw,
#   cap_ipc_lock,cap_ipc_owner,cap_sys_module,cap_sys_rawio,cap_sys_chroot,
#   cap_sys_ptrace,cap_sys_pacct,cap_sys_admin,cap_sys_boot,cap_sys_nice,
#   cap_sys_resource,cap_sys_time,cap_sys_tty_config,cap_mknod,cap_lease,
#   cap_audit_write,cap_audit_control,cap_setfcap,cap_mac_override,
#   cap_mac_admin,cap_syslog,cap_wake_alarm,cap_block_suspend,cap_audit_read,
#   cap_perfmon,cap_bpf,cap_checkpoint_restore
# Ambient set =
# uid=<uid>(<user>) euid=<uid>(<user>)
# gid=<gid>(<group>)

# Exercício 2
# Compilar o webserver
gcc webserver.c -o webserver

# Testar com a porta 4050 (porta não privilegiada)
./webserver 4050
# Resultado: Success: binded to port 4050

# Exercício 3
# Testar com a porta 80 (porta privilegiada < 1024)
./webserver 80
# Resultado: Error on bind: Permission denied
#
# Motivo: portas abaixo de 1024 são consideradas privilegiadas em Linux.
# Um processo sem capabilities especiais (executando como utilizador comum)
# não tem permissão para fazer bind a estas portas.
# Por defeito, o executável não possui capabilities:
getcap ./webserver
# (sem output — nenhuma capability definida)
#
# Como usar capabilities para resolver o problema:
# Em vez de correr o programa como root (setuid root), podemos atribuir
# a capability CAP_NET_BIND_SERVICE especificamente ao executável:
sudo setcap 'cap_net_bind_service=+ep' ./webserver
getcap ./webserver
# ./webserver cap_net_bind_service=ep
./webserver 80
# Resultado: Success: binded to port 80
#
# Com a capability cap_net_bind_service atribuída ao ficheiro executável,
# o programa pode fazer bind a portas < 1024 sem precisar de privilégios root completos.
# Esta abordagem respeita o princípio do menor privilégio: o executável recebe
# apenas a capability estritamente necessária, ao contrário do setuid root
# que concederia todos os privilégios de root ao processo.
