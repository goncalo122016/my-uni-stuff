#!/bin/bash

# Nota: instalar dependências se necessário: sudo apt install acl

# Exercício 1
getfacl porto.txt
# # file: porto.txt
# # owner: <utilizador>
# # group: <utilizador>
# user::r-x
# group::---
# other::---

# Exercício 2
# Definir permissão de escrita para o grupo grupo-ssi via ACL estendida
setfacl -m g:grupo-ssi:w porto.txt

# Exercício 3
getfacl porto.txt
# # file: porto.txt
# # owner: <utilizador>
# # group: <utilizador>
# user::r-x
# group::---
# group:grupo-ssi:-w-
# mask::-w-
# other::---
#
# Diferenças face ao ponto 1:
# Foi adicionada uma entrada ACL específica para o grupo grupo-ssi com permissão de escrita (-w-).
# Surgiu também a linha 'mask', que representa o limite máximo de permissões efetivas
# para utilizadores e grupos definidos via ACL (exceto o dono e others).
# A mask é calculada automaticamente como a união das permissões ACL definidas.

# Exercício 4
su afonso
echo "Novo conteúdo sobre o Porto" >> porto.txt
# Consegue escrever porque o grupo grupo-ssi tem permissão de escrita via ACL.
cat porto.txt
# Resultado: Permission denied
# Apesar de ter permissão de escrita, o utilizador 'afonso' não tem permissão
# de leitura sobre porto.txt (a ACL apenas concedeu escrita).
# Escrever e ler são permissões independentes — sem leitura explícita, cat falha.
