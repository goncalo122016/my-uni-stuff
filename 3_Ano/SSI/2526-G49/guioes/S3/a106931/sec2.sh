#!/bin/bash

# Exercício 0
cat /etc/passwd
cat /etc/group

# Exercício 1
# Criar um utilizador por membro da equipa
sudo adduser afonso
sudo adduser goncalo

# Exercício 2
# Criar grupo com todos os elementos
sudo groupadd grupo-ssi
sudo usermod -aG grupo-ssi afonso
sudo usermod -aG grupo-ssi goncalo

# Criar grupo com apenas 2 elementos
sudo groupadd par-ssi
sudo usermod -aG par-ssi afonso
sudo usermod -aG par-ssi goncalo

# Exercício 3
cat /etc/passwd
cat /etc/group
# Diferenças observadas:
# Em /etc/passwd foram adicionadas entradas para os novos utilizadores criados:
#   afonso:x:1001:1001:,,,:/home/afonso:/bin/bash
#   goncalo:x:1002:1002:,,,:/home/goncalo:/bin/bash
# Em /etc/group foram adicionadas entradas para os novos grupos:
#   grupo-ssi:x:1003:afonso,goncalo
#   par-ssi:x:1004:afonso,goncalo
# E as entradas de grupo primário dos novos utilizadores:
#   afonso:x:1001:
#   goncalo:x:1002:

# Exercício 4
sudo chown afonso braga.txt

# Exercício 5
cat braga.txt
# Resultado: Permission denied
# O ficheiro braga.txt tem permissões 400 (apenas leitura para o dono).
# Como o dono foi alterado para 'afonso', o utilizador atual não tem permissões para ler o ficheiro.

# Exercício 6
su afonso

# Exercício 7
id
# uid=1001(afonso) gid=1001(afonso) groups=1001(afonso),1003(grupo-ssi),1004(par-ssi)
groups
# afonso grupo-ssi par-ssi
# O utilizador 'afonso' tem como grupo primário o seu próprio grupo (afonso),
# e pertence também aos grupos secundários grupo-ssi e par-ssi.

# Exercício 8
cat braga.txt
# Resultado: Braga é uma das cidades mais antigas de Portugal.
# Agora conseguimos ler o ficheiro porque estamos autenticados como 'afonso',
# que é o dono do ficheiro e possui permissão de leitura (400).
# Anteriormente o acesso era negado porque o utilizador corrente não era o dono.

# Exercício 9
cd dir2
# Resultado: bash: cd: dir2: Permission denied
# O exercício 7 da secção 1 removeu as permissões de execução da dir2 para grupo e outros.
# Sem permissão de execução numa diretoria, não é possível entrar nela (cd).
# O utilizador 'afonso' não é o dono da dir2, logo não tem permissão de acesso.
