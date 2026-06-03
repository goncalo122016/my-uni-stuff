#!/bin/bash

# Exercício 1
getfacl porto.txt 
# file: porto.txt
# owner: goncalo
# group: goncalo
# user::r-x
# group::---
# other::---

# Exercício 2
setfacl -m g:grupo-ssi:w porto.txt

# Exercíco 3
getfacl porto.txt 
# file: porto.txt
# owner: goncalo
# group: goncalo
#user::r-x
#group::---
#group:grupo-ssi:-w-
#mask::-w-
#other::---

# Foi criada uma entrada ACL específica para o grupo grupo-ssi. Mas também foi criada a linha mask, que define o limite máximo de permissões efetivas para utilizadores e grupos definidos via ACL.

# Exercício 3
su anibal
echo "Novo conteúdo sobre o Porto" > porto.txt
cat porto.txt 
# cat: porto.txt: Permission denied
# O utilizador pertencente ao grupo consegue modificar o ficheiro devido à permissão de escrita concedida, mas não consegue ler o conteúdo pois não possui permissão de leitura explícita.
