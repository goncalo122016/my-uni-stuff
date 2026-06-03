#!/bin/bash

# Exercício 1
touch lisboa.txt porto.txt braga.txt
echo "Lisboa é a capital de Portugal." > lisboa.txt
echo "Porto é conhecida pelo vinho do Porto." > porto.txt
echo "Braga é uma das cidades mais antigas de Portugal." > braga.txt

# Exercício 2
ls -l lisboa.txt

# Exercício 3
# Dono, grupo e outros com permissões de leitura e escrita (rw-rw-rw-)
chmod 666 lisboa.txt

# Exercício 4
# Dono com leitura e execução, sem escrita (r-x-------)
chmod 500 porto.txt

# Exercício 5
# Apenas o dono com permissão de leitura (r--------)
chmod 400 braga.txt

# Exercício 6
mkdir -p dir1 dir2
ls -ld dir1 dir2

# Exercício 7
# Remover permissão de execução de grupo e outros na dir2
chmod go-x dir2
