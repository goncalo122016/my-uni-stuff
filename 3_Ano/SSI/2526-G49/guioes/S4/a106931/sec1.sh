#!/bin/bash

# Exercício 1 - Setup e execução do programa vulnerável
gcc -o backupssi backupssi.c
sudo chown root:root backupssi
sudo chmod 4755 backupssi

# Executar como utilizador normal:
./backupssi
# Output esperado:
# Directory FD is 3
# mkdir /root/backupssi: File exists   (ou sucesso na 1a execução)
# $ (shell lançada como utilizador normal)

# Exercício 2 - Análise da vulnerabilidade (capability leaking)
#
# O programa abre /root com O_RDONLY enquanto corre com EUID=0 (root via setuid).
# O FD resultante (ex: 3) dá acesso à diretoria /root.
# Após mkdir, o programa larga os privilégios com setuid(getuid()),
# ficando com o UID real do utilizador comum.
# MAS o FD 3 continua aberto e herdado pela shell lançada com execve.
# A shell corre como utilizador normal mas AINDA TEM acesso a /root via FD 3.
# Isto é capability leaking: um recurso privilegiado (FD aberto para /root)
# "vaza" para um contexto sem privilégios.

# Exercício 3 - Demonstração do exploit (dentro da shell lançada por backupssi)
gcc -o exploit1 exploit1.c

# Na shell obtida ao correr ./backupssi, executar:
#   ./exploit1 3
# O exploit usa fchdir(3) para entrar em /root e fdopendir(3) para listar
# o seu conteúdo — tudo isto sem ser root.
# Também pode usar-se diretamente na bash:
#   ls /proc/self/fd/3          -> lista /root
#   cd /proc/self/fd/3          -> entra em /root
#   cat /proc/self/fd/3/.bashrc -> lê ficheiros de /root

# Exercício 4 - Correção
gcc -o backupssi_fix backupssi_fix.c
sudo chown root:root backupssi_fix
sudo chmod 4755 backupssi_fix

# A correção consiste em fechar o FD ANTES de largar privilégios:
#   close(dfd);
#   setuid(getuid());
# Assim, quando execve lança a shell, o FD já não existe — a shell não
# herda qualquer acesso privilegiado a /root.
# Alternativa equivalente: definir O_CLOEXEC ao abrir, ou usar
#   fcntl(dfd, F_SETFD, FD_CLOEXEC)
# para que o FD seja fechado automaticamente no execve.
