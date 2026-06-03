#!/bin/bash

# Exercício 1 - Setup e execução do programa vulnerável
gcc -o passwdleak passwdleak.c
sudo chown root:root passwdleak
sudo chmod 4755 passwdleak

# Executar como utilizador normal:
./passwdleak
# Output esperado:
# Passwd FD leaked: 3
# $ (shell lançada como utilizador normal)

# Exercício 2 - Análise da vulnerabilidade
#
# O programa abre /etc/passwd com O_WRONLY | O_APPEND enquanto corre como root.
# O FD resultante permite escrita no ficheiro crítico /etc/passwd.
# Após setuid(getuid()), o processo perde privilégios root,
# MAS o FD fica aberto e é herdado pela shell (execl não fecha FDs por omissão).
# A shell corre como utilizador normal mas tem um FD com acesso de escrita
# a /etc/passwd — ficheiro que normalmente só root pode escrever.

# Exercício 3 - Exploit (executar dentro da shell lançada por passwdleak)
#
# O FD impresso (ex: 3) aponta para /etc/passwd com permissão de escrita.
# Dentro da shell obtida, basta escrever via /proc/self/fd/<fd>:
#
#   echo "ssihacker::0:0::/root:/bin/sh" >> /proc/self/fd/3
#
# Isto adiciona uma entrada em /etc/passwd com UID=0 e GID=0 (root)
# e sem password, para o utilizador "ssihacker".

# Exercício 4 - Implicações práticas do exploit
#
# Após adicionar a entrada ssihacker::0:0::/root:/bin/sh ao /etc/passwd:
#   su ssihacker   (sem necessidade de password)
#
# O utilizador ssihacker tem UID=0 e GID=0, ou seja, é tratado pelo sistema
# como root. Isto permite:
#   - Ler, escrever e apagar qualquer ficheiro do sistema
#   - Criar/remover utilizadores
#   - Instalar software ou alterar configurações do sistema
#   - Comprometer completamente o sistema operativo
# Em suma, o atacante obtém controlo total (root) sobre a máquina.

# Exercício 5 - Correção
gcc -o passwdleak_fix passwdleak_fix.c
sudo chown root:root passwdleak_fix
sudo chmod 4755 passwdleak_fix

# A correção usa O_CLOEXEC ao abrir /etc/passwd:
#   int fd = open("/etc/passwd", O_WRONLY | O_APPEND | O_CLOEXEC);
# Com esta flag, o kernel fecha automaticamente o FD quando execl/execve
# é chamado, impedindo que a shell herde o acesso de escrita.
# Alternativa equivalente: chamar close(fd) explicitamente antes de execl.
# O princípio subjacente é o mesmo que mitiga o capability leaking:
# nunca deixar recursos privilegiados abertos ao transitar para um
# contexto sem privilégios.
