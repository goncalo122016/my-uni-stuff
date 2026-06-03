#!/bin/bash

# Exercício 1
touch lisboa.txt braga.txt porto.txt
echo "Texto sobre Lisboa" > lisboa.txt
echo "Texto sobre Porto" > porto.txt
echo "Texto sobre Braga" > braga.txt

# Exercício 2
ls -l lisboa.txt

#Exercício 3
chmod u+rw,g+rw,o+rw lisboa.txt

chmod 666 lisboa.txt

#Exercício 4
chmod u=rx porto.txt

chmod 500 porto.txt

#Exercício 5
chmod u=r,go= braga.txt

chmod 400 braga.txt

#Exercício 6
mkdir dir1 dir2
ls -ld dir1 dir2

#Exercício 7
chmod g-x,o-x dir2

chmod 700 dir2


