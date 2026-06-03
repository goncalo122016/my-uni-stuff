#!/bin/bash

# Nota: executar 'exit' para terminar a sessão do utilizador anterior

# Exercício 1
# Compilar o programa que lê um ficheiro passado como argumento
gcc sec3.c -o sec3

# Exercício 2
sudo adduser userssi

# Exercício 3
sudo chown userssi sec3
sudo chown userssi braga.txt

# Exercício 4
./sec3 braga.txt
# Resultado: Erro ao abrir ficheiro: Permission denied
# O executável corre com o UID do utilizador atual (não userssi).
# O ficheiro braga.txt tem permissões 400 e pertence a userssi,
# logo o utilizador atual não tem permissão para o ler.

# Exercício 5
# Definir a permissão setuid no executável
sudo chmod u+s sec3

# Exercício 6
./sec3 braga.txt
# Resultado: Braga é uma das cidades mais antigas de Portugal.
# Com o bit setuid ativo, o processo passa a executar com o UID efetivo do dono
# do executável (userssi), independentemente de quem o invoca.
# Como userssi é dono do braga.txt e tem permissão de leitura (400),
# o programa consegue agora ler o ficheiro com sucesso.
