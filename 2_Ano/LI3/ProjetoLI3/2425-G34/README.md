# Laboratórios de Informática III (2024/2025)

* Afonso Paulo Martins - A106931 - AfonsoMartins26
* Gonçalo José Vieira de Castro - A107337 - goncalo122016
* Luís Miguel Jerónimo Felício - A106913 - luisfelicio

## Como Compilar e Executar o Projeto

Este projeto contém duas partes principais: **programa-principal** e **programa-testes**. Siga os passos abaixo para compilar e executar corretamente o projeto.

### Pré-requisitos
- Certifique-se de que possui as seguintes dependências instaladas:
  - `glib`
  - `gcc` (ou outro compilador compatível com C)
  - Makefile para facilitar a automação da compilação

### Passos para Compilar

1. **Limpar Compilação Anterior**  
   Execute o seguinte comando para limpar ficheiros de builds anteriores:
   ```bash
   make clean
   ```

   Este comando quaisquer ficheiros temporários e binários previamente compilados.

2. **Compilar o Projeto**  
   Para compilar todas as partes do projeto, basta executar:
   ```bash
   make
   ```

   Este comando irá:
   - Compilar todos os módulos do projeto, isto é todos os ficheiros `.c` em `src/`
   - Gerar dois executáveis: `programa-principal` e `programa-testes`

### Executar o Programa de Pricipal

Após a compilação bem-sucedida, pode executar o programa principal, que irá executar os comandos e as respetivas querie, escrevendo os resultados na pasta `resultados/`. Para isso, execute o seguinte comando:

```bash
./programa-principal <caminho_para_dataset> <caminho_para_ficheiro_de_input>
```

Exemplo:

```bash
./programa-principal  dataset/sem_erros/ dataset/inputs_exemplo.txt
```

Este comando:
- **Argumento 1**: O caminho para a diretoria contendo os dados (`dataset/sem_erros/`)
- **Argumento 2**: O caminho para o ficheiro com os comandos a serem processados (`dataset/inputs_exemplo.txt`)

### Executar o Programa de Testes

Após a compilação bem-sucedida, pode executar o programa de testes. Este compara os outputs geradas com os resultados esperados. Para isso, execute o seguinte comando:

```bash
./programa-testes <caminho_para_dataset> <caminho_para_ficheiro_de_input> <caminho_para_resultados_esperados>
```

Exemplo:

```bash
./programa-testes dataset/sem_erros/ dataset/inputs_exemplo.txt resultados-esperados/
```

Este comando:
- **Argumento 1**: O caminho para a diretoria contendo os dados (`dataset/sem_erros/`)
- **Argumento 2**: O caminho para o ficheiro com os comandos a serem processados (`dataset/inputs_exemplo.txt`)
- **Argumento 3**: A diretoria onde estão os resultados esperados (`resultados-esperados/`)

### Interpretação dos Resultados

Após a execução do `programa-testes`, verá um resumo de testes, indicando se os resultados gerados estão corretos. Exemplo de saída:

```
Q1: 25 de 25 testes ok!
Q2: 25 de 25 testes ok!
Q3: 25 de 25 testes ok!
Memória utilizada: 875 MB
Tempos de execução:
        Q1: 160.8 ms
        Q2: 44.1 ms
        Q3: 105.4 ms
Tempo total: 34.3 s
```

- **Q1, Q2, Q3**: Quantidade de testes que passaram em cada tipo de query.
- **Descrepâncias**: Detalhes sobre onde ocorreram as diferenças nos resultados.
- **Memória Utilizada**: Quantidade de memória usada durante a execução.
- **Tempos de Execução**: Tempo que cada query demorou para ser executada.
- **Tempo total**: Tempo total de execução do programa

### Limpar o Projeto

Para remover os ficheiros compilados e começar do zero, utilize:

```bash
make clean
```

## Autotest.py

### Descrição

O `autotest.py` é um script Python que automatiza a construção e a execução de um programa em C, além de validar as saídas contra os resultados esperados.

### Executando o Código Python

Para executar o script, utiliza o seguinte comando:

```bash
python3 script.py <código_fonte> <dataset>
```
Certifica que a estrutura do projeto segue o formato abaixo: 

```bash
/trabalho-pratico
│
├── /include
│   ├── bibliotecas.h
│   └── outros_ficheiros.h
│
├── /resultados
│   └── dados         
│
├── /resultados-esperados
│   └── dados esperados
│
├── /src
│   └── código-fonte.c     
│
├── Makefile 
│
├── programa-principal (depois do make)
├── programa-testes (depois do make)
│
├── relatorio-fase1.pdf
│
├── relatorio-fase2.pdf
└── Autotest.py
```

### Detalhes de Uso
- Código-fonte: Coloca todos os ficheiros de código C na . pasta /codigo_fonte.
- O ficheiro principal deve ser nomeado como programa-principal.c e deve conter um Makefile para a construção do programa.

### Dataset: Crie a pasta /dataset, que deve conter:

- data/: Uma subpasta com os ficheiros de dados que o programa irá utilizar.
- input.txt: O ficheiro que contém as entradas que o programa deverá processar.
- outputs/: Uma subpasta onde se colocará os ficheiros de saída esperados para cada teste.
- Execução: Após estruturar o projeto, podes executar o script autotest.py para compilar o programa e validar as saídas.


## Uso da biblioteca ncurses.h

### Neste trabalho também vamos usar a bibliioiteca ncurses que serve para fazer um 'User Interface' ou seja algo que o user consiga ver, isto tudo dentro do nosso terminal 

### Exemplo de uso desta biblioteca no ficheiro ```myapp.c```:

how to run :
```
gcc myapp.c -o myapp -lncurses
./myapp
```

### algumas coisas a lembrar :
isto tem de se meter sempre na main ou na funcao responsável

```bash
initscr() inicializa a biblioteca ncurses.
cbreak() permite a leitura de inputs sem esperar por 'Enter'.
noecho() desativa a exibição dos caracteres inseridos pelo utilizador.
endwin() encerra a ultilização do ncurses e retorna ao modo normal do terminal.
```

Docs para a [biblioteca ncurses.h](http://jbwyatt.com/ncurses.html).

## Format code 

### Vamos usar o ficheiro .clang-format 

- To format a single file:
```
clang-format -i path/to/your/file.c
```

- To format all C files in the directory:
```
find . -name "*.c" -exec clang-format -i {} \;
```
