# 🔧 Compilador Fortran 77 - Guia Completo

> Um compilador educacional para Fortran 77 com geração de código VM stack-based

## 📋 Índice

1. [Como Rodar](#como-rodar-o-compilador)
2. [Como Testar](#como-testar-tudo)
3. [Explicação de Cada Ficheiro](#explicação-de-cada-ficheiro)
4. [Como Funciona](#como-funciona-a-arquitetura)
5. [Exemplos](#exemplos-práticos)
6. [Troubleshooting](#troubleshooting)

---

## 🚀 Como Rodar o Compilador

### Instalação (Primeira Vez)

```bash
# Entrar na pasta
cd ~/uni/3ano/PL

# Criar ambiente virtual
python3 -m venv venv

# Ativar (em Fish Shell)
fish -c "source venv/bin/activate.fish && pip install -r requirements.txt"
```

### Compilar um Programa

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

### Exemplos Rápidos

```bash
# Hello World
python main.py tests/programs/hello.f90
cat tests/programs/hello.vm

# Condicional IF/ELSE
python main.py tests/programs/ifelse.f90

# Loop DO
python main.py tests/programs/factorial.f90
```

---

## 🧪 Como Testar Tudo

### Rodar Todos os Testes

```bash
# Todos os 164 testes
python -m pytest tests/ -v

# Resultado esperado: 164 passed ✅
```

### Testes por Módulo

```bash
# Apenas Lexer (59 testes)
python -m pytest tests/test_lexer.py -v

# Apenas Parser (36 testes)
python -m pytest tests/test_parser.py -v

# Apenas Semântica (28 testes)
python -m pytest tests/test_semantic.py -v

# Apenas CodeGen (31 testes)
python -m pytest tests/test_codegen.py -v

# Apenas Integração (10 testes)
python -m pytest tests/test_integration.py -v
```

### Um Teste Específico

```bash
python -m pytest tests/test_lexer.py::TestKeywords::test_program_keyword -v
```

### Modo Rápido (sem output detalhado)

```bash
python -m pytest tests/ -q
```

---

## 📁 Explicação de Cada Ficheiro

### 🎯 **main.py** - Entrada Principal (407 linhas)

**O que faz**: Interface de linha de comando (CLI) do compilador

**Funções principais**:
- `compile_file()` - Executa as 4 fases de compilação
- `tokenize_file()` - Mostra tokens
- `parse_file()` - Mostra AST(arvore sintatica)
- `semantic_file()` - Mostra análise semântica

**Como usar**:
```bash
python main.py programa.f90              # Compilação completa
python main.py programa.f90 --tokenize   # Ver tokens
python main.py programa.f90 --parse      # Ver AST
python main.py programa.f90 --semantic   # Ver análise
```

**Output**: Ficheiro `.vm` com código da máquina virtual

---

### 🔤 **src/lexer.py** - Tokenização (287 linhas)

**O que faz**: Quebra o código Fortran em tokens (palavras-chave, números, etc.)

**Tecnologia**: PLY (ply.lex) - biblioteca Python para lexer

**Responsabilidades**:
- Reconhecer keywords (PROGRAM, INTEGER, etc.)
- Reconhecer números (42, 3.14, 1.5E-2)
- Reconhecer strings ('hello', "world")
- Reconhecer operadores (+, -, .EQ., .AND., etc.)
- Ignorar comentários (!)
- Case-insensitive para keywords

**Tokens reconhecidos** (47 tipos):
- Literais: NUMBER, REAL_NUMBER, STRING, IDENTIFIER
- Keywords: PROGRAM, END, INTEGER, REAL, LOGICAL, IF, THEN, ELSE, ENDIF, DO, CONTINUE, GOTO, PRINT, READ, TRUE, FALSE
- Operadores: +, -, *, /, .EQ., .NE., .LT., .LE., .GT., .GE., .AND., .OR., .NOT.

**Exemplo**:
```python
Input:  "INTEGER X = 42"
Output: [Token(INTEGER), Token(IDENTIFIER, X), Token(ASSIGN), Token(NUMBER, 42)]
```

---

### 🌳 **src/ast_nodes.py** - Nós da Árvore (223 linhas)

**O que faz**: Define a estrutura da Árvore de Sintaxe Abstrata (AST)

**Nós principais**:
- `Program` - Raiz da árvore
- `Declaration` - Declarações (INTEGER X)
- `Assignment` - Atribuições (X = 5)
- `PrintStatement` - PRINT
- `ReadStatement` - READ
- `IfStatement` - IF/THEN/ELSE/ENDIF
- `DoLoop` - DO loops
- `BinaryOp` - Operações binárias (X + Y)
- `UnaryOp` - Operações unárias (-X, .NOT. X)
- `Variable`, `IntLiteral`, `RealLiteral`, `StringLiteral`, `BooleanLiteral` - Valores

**Exemplo**:
```
Program
  name: 'HELLO'
  statements:
    PrintStatement
      expressions:
        StringLiteral('Hello, World!')
```

---

### 📝 **src/parser.py** - Análise Sintática (340 linhas)

**O que faz**: Constrói a AST a partir dos tokens

**Tecnologia**: PLY (ply.yacc) - biblioteca Python para parser

**Funcionamento**:
1. Recebe tokens do lexer
2. Aplica regras gramaticais
3. Constrói árvore de sintaxe
4. Retorna a AST

**Precedência de operadores** (do menor para maior):
1. `.OR.` - Ou lógico
2. `.AND.` - E lógico
3. `.NOT.` - Não lógico
4. Comparações (.EQ., .NE., .LT., etc.)
5. + e - (adição, subtração)
6. * e / (multiplicação, divisão)

**Exemplo**:
```
Input tokens:  [PROGRAM, IDENTIFIER, INTEGER, ..., END]
Output AST:    Program(name='TEST', statements=[...])
```

---

### ✔️ **src/semantic.py** - Análise Semântica (354 linhas)

**O que faz**: Valida o código - verifica se tudo está correto

**Responsabilidades**:
1. **Tabela de Símbolos** - Lista de variáveis declaradas
2. **Validação de Declarações** - Detecta redeclarações
3. **Validação de Uso** - Detecta variáveis não declaradas
4. **Validação de Labels** - Verifica DO/GOTO/CONTINUE
5. **Avisos** - Variáveis nunca usadas

**Classes principais**:
- `Symbol` - Representa uma variável
- `SemanticError` - Erro ou aviso
- `SemanticAnalyzer` - O analisador

**Exemplo de erro**:
```fortran
PROGRAM BAD
    Y = 5  ! Erro: Y não foi declarada
END
```

**Output**:
```
[ERRO] Linha 2: Variável 'Y' não declarada
```

---

### 💻 **src/codegen.py** - Geração de Código (365 linhas)

**O que faz**: Converte a AST em código para máquina virtual

**Instruções VM geradas**:
- **Stack**: PUSHI (inteiro), PUSHF (real), PUSHS (string)
- **Memória**: LOAD (carregar variável), STORE (guardar variável)
- **Aritmética**: ADD, SUB, MUL, DIV, NEG
- **Comparação**: EQ, NE, LT, LE, GT, GE
- **Lógica**: AND, OR, NOT
- **Controlo**: LABEL, JUMP, JZ (saltar se zero)
- **I/O**: PRINT, READ
- **Sistema**: HALT (terminar)

**Exemplo**:
```
Input AST:    X = 5
Output VM:    PUSHI 5
              STORE X
```

---

## 🏗️ Como Funciona a Arquitetura

### Pipeline de Compilação (4 Fases)

```
┌─────────────────┐
│ Código Fortran  │  programa.f90
└────────┬────────┘
         │
    ┌────▼──────────────────┐
    │ FASE 1: LEXER         │
    │ (src/lexer.py)        │
    │ Tokenização           │
    └────┬──────────────────┘
         │ tokens
         │
    ┌────▼──────────────────┐
    │ FASE 2: PARSER        │
    │ (src/parser.py)       │
    │ Análise Sintática     │
    │ Construir AST         │
    └────┬──────────────────┘
         │ AST
         │
    ┌────▼──────────────────────────┐
    │ FASE 3: SEMÂNTICA             │
    │ (src/semantic.py)             │
    │ Validação                     │
    │ Tabela de Símbolos            │
    └────┬──────────────────────────┘
         │ AST validada
         │
    ┌────▼──────────────────┐
    │ FASE 4: CODE GEN      │
    │ (src/codegen.py)      │
    │ Gerar Instruções VM   │
    └────┬──────────────────┘
         │
    ┌────▼──────────────┐
    │ Código VM         │  programa.vm
    │ (stack-based)     │
    └───────────────────┘
```

### Fluxo de Dados

```
programa.f90
    ↓
[Lexer] → tokens
    ↓
[Parser] → AST
    ↓
[Semantic] → AST validada (ou erros)
    ↓
[CodeGen] → instruções VM
    ↓
programa.vm
```

---

## 📝 Exemplos Práticos

### Exemplo 1: Hello World

**Ficheiro**: `tests/programs/hello.f90`
```fortran
PROGRAM HELLO
    PRINT *, 'Hello, World!'
END
```

**Compilar**:
```bash
python main.py tests/programs/hello.f90
cat tests/programs/hello.vm
```

**Resultado** (`hello.vm`):
```
PUSHS "Hello, World!"
PRINT
HALT
```

---

### Exemplo 2: Calculadora

```fortran
PROGRAM CALC
    INTEGER A, B, C
    A = 10
    B = 20
    C = A + B
    PRINT *, 'Resultado: ', C
END
```

**Ver tokens**:
```bash
python main.py calc.f90 --tokenize
```

**Ver AST**:
```bash
python main.py calc.f90 --parse
```

**Ver análise semântica**:
```bash
python main.py calc.f90 --semantic
```

**Compilar**:
```bash
python main.py calc.f90
cat calc.vm
```

---

### Exemplo 3: Condicional

```fortran
PROGRAM TESTE
    INTEGER X
    X = 5
    IF (X .GT. 0) THEN
        PRINT *, 'Positivo'
    ELSE
        PRINT *, 'Negativo'
    ENDIF
END
```

---

### Exemplo 4: Loop

```fortran
PROGRAM LOOP
    INTEGER I
    DO 10 I = 1, 5
        PRINT *, I
10  CONTINUE
END
```

---

## 🧪 Estrutura de Testes

### 59 Testes - Lexer (`test_lexer.py`)
```
✓ Keywords (PROGRAM, INTEGER, IF, etc.)
✓ Identificadores (X, Y, RESULT)
✓ Números (inteiros, reais, científica)
✓ Strings (aspas simples e duplas)
✓ Operadores (aritméticos, relacionais, lógicos)
✓ Comentários (! até fim de linha)
```

### 36 Testes - Parser (`test_parser.py`)
```
✓ Programas simples
✓ Declarações
✓ Atribuições
✓ PRINT e READ
✓ IF/THEN/ELSE/ENDIF
✓ DO loops
✓ Expressões complexas
```

### 28 Testes - Semântica (`test_semantic.py`)
```
✓ Redeclarações
✓ Variáveis não declaradas
✓ Variáveis não usadas (avisos)
✓ Labels válidos/inválidos
✓ Tabela de símbolos
```

### 31 Testes - CodeGen (`test_codegen.py`)
```
✓ Atribuições
✓ Operações aritméticas
✓ Operações relacionais e lógicas
✓ PRINT e READ
✓ IF/ELSE com labels
✓ DO loops
```

### 10 Testes - Integração (`test_integration.py`)
```
✓ Compilação completa
✓ Comparação com saída esperada
✓ Testes de regressão
```

---

## ⚙️ Tipos de Dados Suportados

- `INTEGER` - Números inteiros
- `REAL` - Números em ponto flutuante  
- `LOGICAL` - Booleanos (.TRUE., .FALSE.)

---

## 🔧 Operadores Suportados

**Aritméticos**: `+` `-` `*` `/`

**Relacionais**: `.EQ.` `.NE.` `.LT.` `.LE.` `.GT.` `.GE.`

**Lógicos**: `.AND.` `.OR.` `.NOT.`

---

## 📋 Statements Suportados

```fortran
PROGRAM name ... END          ! Programa
INTEGER/REAL/LOGICAL var1, var2  ! Declaração
var = expression              ! Atribuição
PRINT *, expr1, expr2, ...    ! Imprimir
READ *, var1, var2, ...       ! Ler
IF (cond) THEN ... ENDIF      ! Condicional
IF (cond) THEN ... ELSE ... ENDIF  ! Condicional com else
DO label var = start, end ... label CONTINUE  ! Loop
GOTO label                    ! Salto incondicional
label CONTINUE                ! Label
```

---

## ✅ Verificação Final

```bash
# Confirmar que tudo funciona:
python -m pytest tests/ -q
# Resultado: 164 passed ✅
```
