#import "cover.typ": cover
#import "template.typ": *


#show: project

#cover(title: "Trabalho Prático - Grupo 24", authors: (
  (name: "Afonso Martins", number: "a106931"), 
  (name: "Guilherme Costa", number: "a107326"), 
    (name: "Gonçalo Castro", number: "a107337")),
  "Maio, 2026")

#show outline: it => {
    show heading: set text(size: 18pt)
    it
}

#{
  show outline.entry.where(level: 1): it => {
    v(5pt)
    strong(it)
  }

  outline(
    title: [Índice], 
    indent: 11pt + 10%, 
  )
}

#v(-0.4em)
#outline(
  title: none,
  target: figure.where(kind: "attachment"),
  indent: n => 1em,
)
  
#set page(numbering: "1", number-align: center)
#counter(page).update(1)

/*
= Resumo

Este relatório descreve o desenvolvimento de um compilador para um subconjunto da linguagem Fortran 77 (ANSI X3.9-1978), realizado no âmbito da unidade curricular de Processamento de Linguagens da Licenciatura em Engenharia Informática. O compilador foi implementado em Python, recorrendo à biblioteca PLY (_Python Lex-Yacc_) para as fases de análise léxica e sintática, e produz código para a máquina virtual académica fornecida no âmbito da disciplina.

O sistema cobre as quatro etapas obrigatórias do projeto: análise léxica, análise sintática (com construção explícita de uma Árvore de Sintaxe Abstrata), análise semântica com tabela de símbolos, e geração de código para a VM. São discutidas as opções de desenho tomadas em cada fase, as limitações conhecidas face ao standard completo do Fortran 77, e os resultados dos testes realizados.

#v(1em)
*Palavras-chave:* compilador, Fortran 77, PLY, análise léxica, análise sintática, AST, geração de código, máquina virtual.
*/

= Introdução

O Fortran (_Formula Translation_) foi a primeira linguagem de programação de alto nível com adoção generalizada, surgindo na década de 1950 como resposta à necessidade de expressar cálculo científico de forma mais próxima da notação matemática. A sua versão de 1977 — Fortran 77, padronizada pelo ANSI sob a norma X3.9-1978 — consolidou um conjunto estável de construções que continuam a ser relevantes no estudo de técnicas de compilação, nomeadamente pela simplicidade da gramática, pela tipagem estática, e pelo uso explícito de labels e instruções de controlo de fluxo como `GOTO` e `DO`.

O presente projeto tem como objetivo a construção de um compilador funcional para um subconjunto representativo do Fortran 77, capaz de traduzir programas válidos em código executável pela máquina virtual académica disponibilizada na disciplina. O trabalho cobre as quatro etapas clássicas de um compilador de estrutura simples:

+ *Análise léxica* — decomposição do código fonte numa sequência de _tokens_;
+ *Análise sintática* — validação da estrutura gramatical e construção de uma Árvore de Sintaxe Abstrata (AST);
+ *Análise semântica* — verificação de tipos, declaração de variáveis e coerência de _labels_;
+ *Geração de código* — tradução da AST para instruções da VM.

O compilador foi implementado em Python, tirando partido da biblioteca PLY (_Python Lex-Yacc_), tal como sugerido no decorrer da UC.

= Arquitetura Geral do Compilador

O compilador desenvolvido segue uma arquitetura modular, organizada em várias fases bem definidas do processo de compilação. Cada fase é implementada num módulo independente, promovendo separação de responsabilidades e facilidade de manutenção.

#figure(
  image("images/Código fonte.png", width: 80%),
  caption: [Diagrama da Pipeline de funcionamento geral do Compilador desenvolvido]
)

Os principais módulos são:

- *`lexer.py`:* responsável pela análise léxica, convertendo o código fonte em _tokens_.
- *`parser.py`*: responsável pela análise sintática, construindo a AST (_Abstract Syntax Tree_).
- *`semantic.py`*: realiza análise semântica e validações (ex: tipos, variáveis declaradas).
- *`codegen.py`*: gera código para uma máquina virtual baseada em _stack_.
- *`ast_nodes.py`*: define as estruturas de dados da AST.
- *`main.py`*: interface de linha de comandos que coordena todo o pipeline.

O compilador segue um pipeline sequencial, onde a saída de cada fase serve de entrada para a seguinte.

== Decisões de Design
Foi utilizada a biblioteca PLY (Python Lex-Yacc) para implementar o _lexer_ e o _parser_, permitindo definir a gramática de forma declarativa e gerar automaticamente um *parser LALR(1)*.

Optou-se por suportar sintaxe *free-form*, ao contrário do Fortran 77 clássico (fixed-form), simplificando a implementação e tornando o sistema mais flexível.

A interface de utilização é feita através de linha de comandos (`main.py`), permitindo compilar ficheiros fonte e gerar código VM de forma simples.

= Análise Léxica

A análise léxica é responsável por transformar o código fonte numa sequência de
_tokens_, que serão posteriormente consumidos pelo _parser_. O _lexer_ foi
implementado com PLY (`ply.lex`), em que cada tipo de _token_ é definido por
uma expressão regular sob a forma de função Python ou de variável de _string_,
conforme a complexidade da regra.

== Tokens reconhecidos

O compilador reconhece os seguintes tipos de _tokens_, agrupados por categoria:

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 7pt,
  [*Categoria*], [*Tokens*],
  [Identificadores], [`IDENTIFIER`],
  [Literais inteiros], [`NUMBER` — ex: `42`],
  [Literais reais], [`REAL_NUMBER` — ex: `3.14`, `1.5e-3`],
  [Literais string], [`STRING` — plicas `'...'` ou aspas `"..."`],
  [Literais booleanos], [`TRUE` (`.TRUE.`), `FALSE` (`.FALSE.`)],
  [Palavras reservadas], [`PROGRAM`, `END`, `INTEGER`, `REAL`, `LOGICAL`, `IF`,
   `THEN`, `ELSE`, `ENDIF`, `DO`, `CONTINUE`, `GOTO`, `PRINT`, `READ`,
   `SUBROUTINE`, `FUNCTION`, `CALL`, `RETURN`],
  [Operadores aritméticos], [`PLUS` (`+`), `MINUS` (`-`), `TIMES` (`*`), `DIVIDE` (`/`)],
  [Operadores relacionais], [`EQ` (`.EQ.`), `NE` (`.NE.`), `LT` (`.LT.`),
   `LE` (`.LE.`), `GT` (`.GT.`), `GE` (`.GE.`)],
  [Operadores lógicos], [`AND` (`.AND.`), `OR` (`.OR.`), `NOT` (`.NOT.`)],
  [Delimitadores], [`LPAREN` (`(`), `RPAREN` (`)`), `COMMA` (`,`)],
  [Atribuição], [`ASSIGN` (`=`)],
)

== _Case insensitivity_

O Fortran 77 é _case-insensitive_ — `PROGRAM`, `program` e `Program` são
equivalentes, e `X` e `x` referem-se à mesma variável. Esta característica é
implementada na regra de identificadores, onde o valor lido é convertido para
minúsculas antes de ser pesquisado no dicionário de palavras reservadas:

#set align(center)

```python
def t_IDENTIFIER(self, t):
    r"[a-zA-Z][a-zA-Z0-9_]*"
    t.type = self.reserved.get(t.value.lower(), "IDENTIFIER")
    return t
```

#set align(left)

O valor do _token_ (`t.value`) é preservado com o _case_ original — apenas a
classificação do tipo usa a forma em minúsculas. Assim, uma variável declarada
como `INTEGER MyVar` é armazenada na tabela de símbolos como `"MyVar"`.

== Literais

São suportados quatro tipos de literais:

- *Inteiros* — sequência de dígitos decimais: `42`, `0`, `1000`.
- *Reais* — com ponto decimal ou notação científica: `3.14`, `.5`, `1.5e-3`.
  A regra `t_REAL_NUMBER` é definida antes de `t_NUMBER`, garantindo que `3.14`
  seja reconhecido como `REAL_NUMBER` e não como dois tokens separados.
- *Strings* — delimitadas por plicas (`'Ola, Mundo!'`) ou aspas (`"hello"`). As aspas externas são removidas do valor do _token_.
- *Booleanos* — `.TRUE.` e `.FALSE.`, convertidos para `True`/`False` Python.

== Operadores relacionais e lógicos

O Fortran 77 utiliza operadores delimitados por pontos, distintos dos símbolos
comuns noutras linguagens. Por exemplo, `.EQ.` corresponde a `==` em C. Estes
operadores são reconhecidos por funções dedicadas (e.g., `t_EQ`, `t_AND`), que
têm prioridade sobre a regra genérica de identificadores no PLY. Cada função
aceita tanto maiúsculas como minúsculas em qualquer posição:

#set align(center)

```python
def t_EQ(self, t):
    r"\.[Ee][Qq]\."
    return t

def t_AND(self, t):
    r"\.[Aa][Nn][Dd]\."
    return t
```

#set align(left)

== Comentários e espaços em branco

Comentários são introduzidos pelo carácter `!` e estendem-se até ao fim da
linha. A regra `t_COMMENT` consome a sequência sem devolver nenhum _token_,
tornando os comentários invisíveis para o _parser_:

#set align(center)

```python
def t_COMMENT(self, t):
    r"!.*"
    pass
```

#set align(left)

Espaços, tabulações e retornos de carro são declarados em `t_ignore`:

#set align(center)

```python
t_ignore = " \t\r"
```

#set align(left)

Quebras de linha são tratadas por `t_newline`, que incrementa o contador de
linha interno do _lexer_ (`t.lexer.lineno`) para que as mensagens de erro
reportem a linha correta.

== Prioridade das regras

O PLY determina a prioridade das regras da seguinte forma: funções têm
prioridade sobre variáveis de string, e entre funções a ordem é a de
declaração no ficheiro. Isto é relevante em dois pontos:

- `t_REAL_NUMBER` é declarada antes de `t_NUMBER`, garantindo que `3.14` não
  seja tokenizado como `3`, `.`, `14`.
- As funções dos operadores com pontos (`.EQ.`, `.AND.`, etc.) são declaradas
  antes de `t_IDENTIFIER`, evitando que `.EQ.` seja lido como `.` seguido do
  identificador `EQ` seguido de `.`.
/*
== Exemplo de tokenização

Para o programa:

#set align(center)

```python
PROGRAM HELLO
    PRINT *, 'Ola, Mundo!'
END
```

#set align(left)

O _lexer_ produz a seguinte sequência de _tokens_:

#table(
  columns: (auto, auto, auto),
  stroke: 0.5pt,
  inset: 7pt,
  [*Tipo*], [*Valor*], [*Linha*],
  [`PROGRAM`],   [`PROGRAM`],       [1],
  [`IDENTIFIER`],[`HELLO`],         [1],
  [`PRINT`],     [`PRINT`],         [2],
  [`TIMES`],     [`*`],             [2],
  [`STRING`],    [`Ola, Mundo!`],   [2],
  [`END`],       [`END`],           [3],
)
*/

= Análise Sintática

A análise sintática verifica se a sequência de _tokens_ produzida pelo _lexer_
respeita a gramática da linguagem, construindo simultaneamente uma
representação estruturada do programa sob a forma de uma _Abstract Syntax Tree_
(AST). O _parser_ foi implementado com PLY (`ply.yacc`), que gera
automaticamente um _parser_ LALR(1) a partir de regras de produção definidas
como funções Python.

== Gramática implementada

Cada função `p_*` no módulo `parser.py` corresponde a uma ou mais produções.
A gramática completa do subconjunto suportado é a seguinte:

#set align(center)

```python
program         : program_units

program_units   : program_unit
                | program_units program_unit

program_unit    : main_program
                | subroutine
                | function

main_program    : PROGRAM IDENTIFIER statements END
                | PROGRAM IDENTIFIER END

statements      : statement
                | statements statement

statement       : declaration | assignment | print_stmt | read_stmt
                | if_stmt | do_loop | goto_stmt | call_stmt | return_stmt

declaration     : type_spec var_list
type_spec       : INTEGER | REAL | LOGICAL
var_list        : IDENTIFIER
                | var_list COMMA IDENTIFIER

assignment      : IDENTIFIER ASSIGN expression

print_stmt      : PRINT TIMES print_list
print_list      : COMMA expression
                | print_list COMMA expression

read_stmt       : READ TIMES read_list
read_list       : COMMA IDENTIFIER
                | read_list COMMA IDENTIFIER

if_stmt         : IF LPAREN expression RPAREN THEN statements ENDIF
                | IF LPAREN expression RPAREN THEN statements ELSE
                  statements ENDIF

do_loop         : DO NUMBER IDENTIFIER ASSIGN expression COMMA expression
                  statements NUMBER CONTINUE

goto_stmt       : GOTO NUMBER
call_stmt       : CALL IDENTIFIER LPAREN arg_list RPAREN
                | CALL IDENTIFIER
return_stmt     : RETURN

expression      : expr_or
expr_or         : expr_and | expr_or OR expr_and
expr_and        : expr_not | expr_and AND expr_not
expr_not        : expr_rel  | NOT expr_not
expr_rel        : expr_arith
                | expr_arith EQ  expr_arith | expr_arith NE expr_arith
                | expr_arith LT  expr_arith | expr_arith LE expr_arith
                | expr_arith GT  expr_arith | expr_arith GE expr_arith
expr_arith      : term
                | expr_arith PLUS term | expr_arith MINUS term
term            : factor
                | term TIMES factor | term DIVIDE factor
factor          : primary
                | MINUS factor  %prec UMINUS
primary         : NUMBER | REAL_NUMBER | STRING | TRUE | FALSE
                | IDENTIFIER
                | IDENTIFIER LPAREN arg_list RPAREN
                | LPAREN expression RPAREN

arg_list        : expression | arg_list COMMA expression

subroutine      : SUBROUTINE IDENTIFIER LPAREN param_list RPAREN
                  statements END
                | SUBROUTINE IDENTIFIER statements END

function        : type_spec FUNCTION IDENTIFIER LPAREN param_list RPAREN
                  statements END
                | type_spec FUNCTION IDENTIFIER statements END

param_list      : IDENTIFIER | param_list COMMA IDENTIFIER
```

#set align(left)

== Precedência de operadores

A precedência e associatividade foram declaradas explicitamente na tabela
`precedence` do _parser_, da menor para a maior prioridade:

#set align(center)

#table(
  columns: (auto, auto, auto),
  stroke: 0.5pt,
  inset: 7pt,
  [*Nível*], [*Associação*], [*Operadores*],
  [1 (menor)], [esquerda],  [`.OR.`],
  [2],         [esquerda],  [`.AND.`],
  [3],         [direita],   [`.NOT.`],
  [4],         [esquerda],  [`.EQ.` `.NE.` `.LT.` `.LE.` `.GT.` `.GE.`],
  [5],         [esquerda],  [`+` `-`],
  [6],         [esquerda],  [`*` `/`],
  [7 (maior)], [direita],   [Unário `-` (`UMINUS`)],
)

#set align(left)

O nível 7 (`UMINUS`) é uma _pseudo-precedência_ declarada apenas para resolver
a ambiguidade do menos unário. A regra `factor : MINUS factor` usa a diretiva
`%prec UMINUS`, forçando o PLY a tratar o `-` nesse contexto como operador
unário de alta prioridade, distinto do `-` binário do nível 5. Sem esta
declaração, uma expressão como `-2 * 3` poderia ser mal interpretada.

Exemplos de expressões e a sua árvore resultante:

- `1 + 2 * 3` → `BinaryOp(+, IntLiteral(1), BinaryOp(*, IntLiteral(2), IntLiteral(3)))` \
  _multiplicação tem prioridade sobre adição_
- `A .OR. B .AND. C` → `BinaryOp(.OR., Variable(A), BinaryOp(.AND., Variable(B), Variable(C)))` \
  `.AND.` tem prioridade sobre `.OR.`
- `-X + 1` → `BinaryOp(+, UnaryOp(-, Variable(X)), IntLiteral(1))` \
  _unário tem prioridade sobre binário_

== Construção da AST

Durante a análise sintática é construída explicitamente uma AST, usando as
classes definidas em `ast_nodes.py`. Cada regra de produção cria e devolve o
nó correspondente. A hierarquia principal é:

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 7pt,
  [*Classe*], [*Representa*],
  [`Program`],          [Bloco `PROGRAM ... END`],
  [`Declaration`],      [`INTEGER X, Y`],
  [`Assignment`],       [`X = expr`],
  [`PrintStatement`],   [`PRINT *, ...`],
  [`ReadStatement`],    [`READ *, ...`],
  [`IfStatement`],      [`IF (...) THEN ... [ELSE ...] ENDIF`],
  [`DoLoop`],           [`DO label I = start, end ... label CONTINUE`],
  [`GotoStatement`],    [`GOTO label`],
  [`ContinueStatement`],[`label CONTINUE`],
  [`Subroutine`],       [`SUBROUTINE nome(...) ... END`],
  [`Function`],         [`type FUNCTION nome(...) ... END`],
  [`CallStatement`],    [`CALL nome(...)`],
  [`ReturnStatement`],  [`RETURN`],
  [`BinaryOp`],         [Operação binária: `left op right`],
  [`UnaryOp`],          [Operação unária: `op operand`],
  [`Variable`],         [Referência a variável: `X`],
  [`IntLiteral`],       [Inteiro: `42`],
  [`RealLiteral`],      [Real: `3.14`],
  [`StringLiteral`],    [String: `'hello'`],
  [`BooleanLiteral`],   [`.TRUE.` ou `.FALSE.`],
  [`FunctionCall`],     [Chamada de função em expressão: `F(x)`],
)

Por exemplo, a instrução `X = 1 + 2 * 3` produz a subárvore:

#set align(center)

```
Assignment
  target: 'X'
  value:
    BinaryOp(op='+')
      left:  IntLiteral(1)
      right: BinaryOp(op='*')
               left:  IntLiteral(2)
               right: IntLiteral(3)
```

#set align(left)

== Suporte a subprogramas

Para além do programa principal, a gramática suporta a definição de
subprogramas (`SUBROUTINE` e `FUNCTION`) como unidades independentes no mesmo
ficheiro. A regra `program_units` permite que um ficheiro contenha múltiplas
unidades; o _parser_ identifica o nó `Program` (programa principal) e devolve-o
como raiz da AST:

#set align(center)

```python
def p_program(self, p):
    """program : program_units"""
    units = p[1]
    main = next((u for u in units if isinstance(u, Program)), None)
    subprograms = [u for u in units if not isinstance(u, Program)]
    if main is not None:
        main.statements = main.statements + subprograms
        p[0] = main
    else:
        p[0] = units[0] if len(units) == 1 else units
```

#set align(left)

Os nós `Subroutine` e `Function` são incorporados diretamente em
`Program.statements`, de modo a que o parser devolva sempre um único nó
`Program` como raiz da AST. O gerador de código emite os subprogramas após o
`STOP` do programa principal durante a travessia de `statements`.

== Tratamento de erros sintáticos

Quando o _parser_ encontra um _token_ inesperado, é lançada uma exceção
`ParserError` com o número de linha e o valor do _token_ que causou o erro:

#set align(center)

```python
def p_error(self, p):
    if p:
        raise ParserError(
            f"Erro sintático em '{p.value}'",
            lineno=p.lineno,
            value=p.value,
        )
    else:
        raise ParserError("Erro sintático no fim do input")
```

#set align(left)

Não foi implementada recuperação de erros (`error` token do PLY), pelo que o
compilador para na primeira ocorrência. Esta é uma limitação conhecida — uma
extensão futura poderia usar `p_error` com sincronização por tokens de
continuação para reportar múltiplos erros numa única passagem.


= Análise Semântica

A análise semântica valida o significado do programa após a análise sintática,
garantindo que as construções são logicamente corretas além de estruturalmente
válidas. Esta fase foi implementada no módulo `semantic.py`, através da classe
`SemanticAnalyzer`, que percorre a AST em múltiplas passagens.

== Estrutura da tabela de símbolos

Cada variável ou subprograma declarado é representado por um objeto `Symbol`
com os seguintes campos:

#set align(center)

#table(
  columns: (auto, auto, auto),
  stroke: 0.5pt,
  inset: 7pt,
  [*Campo*], [*Tipo*], [*Descrição*],
  [`name`],      [`str`],  [Nome da variável],
  [`type_spec`], [`str`],  [`INTEGER`, `REAL`, `LOGICAL`, `SUBROUTINE`, `PARAM`],
  [`line`],      [`int`],  [Linha de declaração],
  [`used`],      [`bool`], [Se foi referenciada em alguma expressão],
  [`assigned`],  [`bool`], [Se foi alvo de atribuição ou leitura],
)

#set align(left)

A tabela é um dicionário `Dict[str, Symbol]` que mapeia nomes (com o _case_
original) para os respetivos símbolos.

== Passagens de análise

A análise é feita em cinco passagens sequenciais sobre a AST, separando
responsabilidades e garantindo que a informação necessária está disponível em
cada etapa:

+ *`_collect_declarations`* — percorre todos os nós `Declaration`, `Subroutine`
  e `Function`, preenchendo a tabela de símbolos. Os parâmetros formais de
  subprogramas são inseridos com tipo `PARAM`; quando a declaração de tipo
  explícita do parâmetro for encontrada no corpo, o tipo é atualizado:


#set align(center)

```python
  if self.symbols[var_name].type_spec == "PARAM":
      self.symbols[var_name].type_spec = node.type_spec
  ```

#set align(left)


+ *`_collect_labels`* — recolhe todos os labels numéricos em três conjuntos
  distintos: `labels` (labels de `CONTINUE`), `do_labels` (labels de `DO`) e
  `goto_labels` (labels referenciados por `GOTO`). A separação permite validar
  cada caso de forma independente.

+ *`_validate_statements`* — percorre todos os _statements_ verificando uso de
  variáveis não declaradas, marcando variáveis como `used` e `assigned`.

+ *`_check_unused_variables`* — gera avisos (_warnings_) para variáveis
  declaradas mas nunca referenciadas. Avisos não bloqueiam a compilação.

+ *`_validate_goto_labels`* — cruza os três conjuntos de labels para detetar:
  - `GOTO` para label inexistente
  - `DO` sem `CONTINUE` correspondente

== Verificações implementadas

=== Redeclaração de variáveis

É verificado se uma variável é declarada mais do que uma vez no mesmo âmbito.
A exceção é a redeclaração de um parâmetro com tipo explícito, que é permitida
e utilizada para especializar o tipo genérico `PARAM`:

#set align(center)

```python
INTEGER FUNCTION CONVRT(N, B)
    INTEGER N, B   # redeclaração válida: PARAM -> INTEGER
```

#set align(left)

=== Uso de variáveis não declaradas

Qualquer referência a uma variável (em expressões, atribuições, `READ`,
variável de controlo de `DO`) que não conste na tabela de símbolos gera um erro:

#set align(center)

```python
X = 5   # ERRO se X não foi declarado
```

#set align(left)

=== Validação de labels

Os labels são validados em duas direções:

- Todo o `GOTO N` exige que exista um `N CONTINUE` ou um `DO N` no programa.
- Todo o `DO N` exige que exista um `N CONTINUE` correspondente.

Labels duplicados (dois `CONTINUE` com o mesmo número) são também detetados
na segunda passagem.

=== Validação de chamadas a subprogramas

Tanto `CALL nome(...)` como chamadas de função em expressões (`FunctionCall`)
verificam se `nome` está na tabela de símbolos com tipo `SUBROUTINE` ou o tipo
de retorno da função.

=== Variáveis declaradas mas não utilizadas

Gera um aviso (não erro) para cada variável cujo campo `used` permaneça `False`
após a análise. Isto ajuda a identificar declarações redundantes sem bloquear
a compilação.

=== Verificação de tipos em expressões

O analisador semântico verifica a compatibilidade de tipos nas expressões e nas
atribuições, detetando usos incorretos de LOGICAL em contexto aritmético e
misturas de tipos incompatíveis. A inferência de tipos é feita pelo método
`_get_expr_type`, que devolve `"INTEGER"`, `"REAL"`, `"LOGICAL"` ou `None`
(tipo desconhecido/erro) para qualquer expressão.

As regras aplicadas são:

#table(
  columns: (auto, auto, 1fr),
  stroke: 0.5pt,
  inset: 7pt,
  [*Contexto*], [*Situação*], [*Resultado*],
  [Ops. aritméticas \ `+` `-` `*` `/`],
    [Operando LOGICAL],
    [*Erro* — LOGICAL não pode ser usado em contexto aritmético],
  [Ops. aritméticas \ `+` `-` `*` `/`],
    [Mistura INTEGER e REAL],
    [*Aviso* — possível perda de precisão],
  [`.AND.` `.OR.`],
    [Operando não-LOGICAL],
    [*Erro* — operação lógica exige LOGICAL],
  [`.NOT.`],
    [Operando não-LOGICAL],
    [*Erro* — `.NOT.` exige LOGICAL],
  [Negação unária `-`],
    [Operando LOGICAL],
    [*Erro* — não se pode negar aritmeticamente um LOGICAL],
  [`.LT.` `.LE.` `.GT.` `.GE.`],
    [Operando LOGICAL],
    [*Erro* — LOGICAL não tem ordem definida],
  [Atribuição],
    [LOGICAL atribuído a INTEGER ou REAL],
    [*Erro* — tipos incompatíveis],
  [Atribuição],
    [INTEGER ou REAL atribuído a LOGICAL],
    [*Erro* — tipos incompatíveis],
  [Atribuição],
    [REAL atribuído a INTEGER],
    [*Aviso* — possível truncamento],
)

= Geração de Código

A geração de código converte a AST validada em instruções para a máquina
virtual _stack-based_ fornecida na disciplina. Esta fase está implementada no
módulo `codegen.py`, através da classe `CodeGenerator`.

== Modelo de execução da VM

A máquina virtual opera com uma _stack_ de operandos e uma região de memória
global para variáveis. O modelo de execução é:

- valores são empilhados com instruções `PUSH*`;
- operações aritméticas, lógicas e relacionais consomem operandos do topo e
  empilham o resultado;
/*
== Instruções emitidas

#table(
  columns: (auto, 1fr),
  stroke: 0.5pt,
  inset: 7pt,
  [*Instrução*], [*Descrição*],
  [`PUSHN n`],        [Aloca _n_ posições na memória global (uma por variável)],
  [`START`],          [Marca o início da execução],
  [`STOP`],           [Termina o programa principal],
  [`PUSHI n`],        [Empilha inteiro _n_],
  [`PUSHF x`],        [Empilha real _x_],
  [`PUSHS "str"`],    [Empilha string _str_],
  [`PUSHG addr`],     [Empilha valor da variável global em _addr_],
  [`STOREG addr`],    [Guarda topo da stack na variável global _addr_],
  [`PUSHL idx`],      [Empilha variável local no índice _idx_ do frame de ativação],
  [`STOREL idx`],     [Guarda topo da stack na variável local _idx_ do frame],
  [`ADD SUB MUL DIV`],[Operações aritméticas inteiras],
  [`EQUAL`],          [Igualdade: empilha 1 se iguais, 0 caso contrário],
  [`INF INFEQ`],      [Menor que / menor ou igual],
  [`SUP SUPEQ`],      [Maior que / maior ou igual],
  [`AND OR NOT`],     [Operações lógicas],
  [`SWAP`],           [Troca os dois elementos do topo],
  [`JZ label`],       [Salta para _label_ se topo for zero],
  [`JUMP label`],     [Salto incondicional],
  [`READ`],           [Lê uma linha de texto e empilha como string],
  [`ATOI`],           [Converte string do topo para inteiro],
  [`ATOF`],           [Converte string do topo para real],
  [`WRITEI`],         [Imprime inteiro do topo],
  [`WRITEF`],         [Imprime real do topo],
  [`WRITES`],         [Imprime string do topo],
  [`WRITELN`],        [Emite newline],
  [`PUSHA label`],    [Empilha endereço de código do _label_],
  [`CALL`],           [Chama subprograma cujo endereço está no topo],
  [`RETURN`],         [Retorna do subprograma],
)
*/
== Alocação de variáveis

O gerador distingue dois tipos de variáveis:

- *Globais* — declaradas no corpo do programa principal; acedidas com `PUSHG`/`STOREG`. Alocadas com `PUSHN` antes de `START`.
- *Locais* — declaradas dentro de subroutines ou functions; acedidas com `PUSHL`/`STOREL` no frame de ativação correspondente.

A separação é feita em `_collect_globals`, que para na fronteira dos subprogramas, e `_build_local_frame`, que constrói o mapa de índices locais para cada subprograma.

#set align(center)

```python
PUSHN 3    # reserva espaço para 3 variáveis globais
START
```

#set align(left)

== Geração por construção

=== Atribuição

A expressão é avaliada (empilhando o resultado) e depois armazenada:

#set align(center)

```python
X = 5 + 3
```

#set align(left)

Gera:

#set align(center)

```
PUSHI 5
PUSHI 3
ADD
STOREG 0    # endereço de X
```

#set align(left)

=== Expressões aritméticas

As expressões são geradas por* travessia recursiva pós-ordem* da AST, garantindo
que os operandos são empilhados antes da operação:

#set align(center)

```python
X = (1 + 2) * 3
```

#set align(left)

Gera:

#set align(center)

```
PUSHI 1
PUSHI 2
ADD
PUSHI 3
MUL
STOREG 0
```

#set align(left)

=== Menos unário (-)

O menos unário não tem instrução direta na VM. É implementado com `SWAP/SUB`:

#set align(center)

```python
X = -Y
```

#set align(left)
#set align(center)

```
PUSHG 1     # Y
PUSHI 0
SWAP
SUB         # 0 - Y
STOREG 0
```

#set align(left)

=== Operadores relacionais e lógicos

Os operadores Fortran são mapeados para as instruções equivalentes da VM:
#set align(center)
#table(
  columns: (auto, auto),
  stroke: 0.5pt,
  inset: 7pt,
  [*Operador Fortran*], [*Instruções VM*],
  [`.EQ.`], [`EQUAL`],
  [`.NE.`], [`EQUAL` + `NOT`],
  [`.LT.`], [`INF`],
  [`.LE.`], [`INFEQ`],
  [`.GT.`], [`SUP`],
  [`.GE.`], [`SUPEQ`],
  [`.AND.`],[`AND`],
  [`.OR.`], [`OR`],
  [`.NOT.`],[`NOT`],
)
#set align(left)

=== Estrutura condicional

O `IF-THEN-ELSE-ENDIF` é compilado com dois labels gerados dinamicamente
(`ELSE_n` e `ENDIF_n`), garantindo que não colidem com os labels numéricos do Fortran:

#set align(center)

```python
IF (X .GT. 0) THEN
    X = X + 1
ELSE
    X = X - 1
ENDIF
```

#set align(left)
Gera:
#set align(center)

```python
PUSHG 0         # X
PUSHI 0
SUP             # X > 0
JZ ELSE1
PUSHG 0
PUSHI 1
ADD
STOREG 0
JUMP ENDIF2
ELSE1:
PUSHG 0
PUSHI 1
SUB
STOREG 0
ENDIF2:
```

#set align(left)

=== Ciclo DO

O `DO` é compilado como um ciclo com verificação de condição no início (`INFEQ`) e incremento explícito da variável de controlo:

#set align(center)

```python
DO 10 I = 1, 5
    PRINT *, I
10 CONTINUE
```

#set align(left)

Gera:

#set align(center)

```python
PUSHI 1
STOREG 0        # I = 1
LOOP1:
PUSHG 0         # I
PUSHI 5
INFEQ           # I <= 5
JZ ENDLOOP2
PUSHG 0
WRITEI
WRITELN
PUSHG 0         # incremento no CONTINUE
PUSHI 1
ADD
STOREG 0
JUMP LOOP1
ENDLOOP2:
```

#set align(left)

=== Entrada e saída (I/O)

`PRINT` emite uma instrução de escrita por expressão, seguida de `WRITELN`. A
instrução de escrita é escolhida pelo tipo real da expressão:

- `StringLiteral` → `WRITES`
- `RealLiteral` → `WRITEF`
- `Variable` → consulta `symbol_table.get_type(nome)`: se `REAL` emite `WRITEF`, caso contrário `WRITEI`
- Restantes expressões → `WRITEI`

`READ` lê sempre como string e converte com `ATOF` para variáveis `REAL` ou `ATOI` para `INTEGER`/`LOGICAL`, consultando o tipo na tabela de símbolos.

=== Subprogramas e frames de ativação

Os subprogramas (`SUBROUTINE` e `FUNCTION`) são emitidos após o `STOP` do
programa principal. A passagem de argumentos e o acesso a variáveis locais são
feitos através de *frames de ativação*, isolando completamente o estado de cada
chamada do espaço global.

==== Convenção de chamada

+ O chamador empilha os argumentos em ordem, sem escrever em variáveis globais.
+ `PUSHA nome` / `CALL` transfere o controlo e cria um novo frame.
+ Dentro do subprograma, os parâmetros estão em `PUSHL 0`, `PUSHL 1`, ...; as
  variáveis locais declaradas seguem nos índices seguintes, alocadas com `PUSHN`.
+ Para `FUNCTION`, o nome da função é tratado como uma variável local extra
  (último slot do frame): o corpo atribui-lhe o resultado e, antes do `RETURN`,
  o slot é empurrado para a stack.
+ `RETURN` restaura o frame anterior; para funções, o valor de retorno fica no
  topo da stack.

==== Exemplo — FUNCTION com frame de ativação

#set align(center)

```python
INTEGER FUNCTION ADD(A, B)
INTEGER A, B
ADD = A + B
END
```

#set align(left)

Gera:

#set align(center)

```
ADD:
PUSHN 1      # slot para variável de retorno (ADD)
PUSHL 0      # A
PUSHL 1      # B
ADD
STOREL 2     # ADD = A + B
PUSHL 2      # empurra valor de retorno antes do RETURN
RETURN
```

#set align(left)

No lado do _caller_, os argumentos são empilhados antes de `PUSHA`/`CALL`; após
o retorno, o valor de retorno está no topo da _stack_ e pode ser usado diretamente
pela expressão que invocou a função.

Esta abordagem elimina conflitos de nomes entre variáveis locais e globais e
torna as chamadas seguras para recursão.

= Executar o Compilador

== Instalação (Primeira Vez)

```bash
# Entrar na pasta base do repositório
cd PL_Grupo24

# Criar ambiente virtual
python3 -m venv venv

# Ativar (Linux)
source venv/bin/activate

# Ativar ambiente virtual (Windows CMD)
venv\Scripts\activate.bat

# Instalar dependências
pip install -r requirements.txt
```

== Compilar um Programa

```bash
# Compilação básica (gera .vm)
python main.py programa.f90

# Ver apenas tokens (tokenização)
python main.py programa.f90 --tokenize

# Ver árvore sintática (AST)
python main.py programa.f90 --parse

# Ver análise semântica (tabela de símbolos)
python main.py programa.f90 --semantic

# Modo verbose (mais detalhes)
python main.py programa.f90 -v
```

=== Exemplos Rápidos

```bash
# Hello World
python main.py tests/programs/hello.f90
cat tests/programs/hello.vm

# Condicional IF/ELSE
python main.py tests/programs/ifelse.f90

# Loop DO
python main.py tests/programs/factorial.f90
```

== Como Testar Tudo

=== Rodar Todos os Testes

```bash
# Todos os 179 testes
python -m pytest tests/ -v

# Resultado esperado: 179 passed
```

=== Testes por Módulo

```bash
# Apenas Lexer (59 testes)
python -m pytest tests/test_lexer.py -v

# Apenas Parser (36 testes)
python -m pytest tests/test_parser.py -v

# Apenas Semântica (43 testes)
python -m pytest tests/test_semantic.py -v

# Apenas CodeGen (31 testes)
python -m pytest tests/test_codegen.py -v

# Apenas Integração (10 testes)
python -m pytest tests/test_integration.py -v
```

= Conclusões e Trabalho Futuro

O desenvolvimento deste compilador permitiu implementar de forma integrada as
principais fases clássicas de um compilador: análise léxica, análise
sintática, análise semântica e geração de código para uma máquina virtual.
O sistema desenvolvido
é capaz de processar programas com declarações, expressões, estruturas
condicionais, ciclos e subprogramas.
A construção
explícita de uma AST simplificou significativamente as fases posteriores do
compilador, especialmente a análise semântica e a geração de código.

A arquitetura modular adotada contribuiu para uma separação clara entre fases,
tornando o sistema mais organizado, extensível e fácil de testar. Além disso,
o projeto permitiu consolidar conceitos fundamentais relacionados com gramáticas
LALR(1), tabelas de símbolos, validação semântica e tradução para código
executável.

Como trabalho futuro, seria interessante adicionar suporte a arrays, strings de
comprimento variável e outras construções clássicas do Fortran 77, bem como
introduzir mecanismos de recuperação de erros sintáticos e otimização do código
gerado (e.g. eliminação de código morto, _constant folding_).

No geral, o projeto cumpriu os objetivos propostos e proporcionou uma visão
prática e aprofundada do funcionamento interno de um compilador completo.