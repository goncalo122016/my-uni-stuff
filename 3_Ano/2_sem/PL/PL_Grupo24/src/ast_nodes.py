"""
Nós de Árvore de Sintaxe Abstrata (AST) para Fortran 77

Cada classe representa um tipo de nó na AST.
"""


class ASTNode:
    """Classe base para todos os nós AST"""

    def __init__(self, lineno=None):
        self.lineno = lineno

    def __repr__(self):
        attrs = ", ".join(
            f"{k}={repr(v)}" for k, v in self.__dict__.items() if k != "lineno"
        )
        return f"{self.__class__.__name__}({attrs})"

class Program(ASTNode):
    """Nó raiz: PROGRAM nome ... END"""

    def __init__(self, name, statements, lineno=None):
        super().__init__(lineno)
        self.name = name  # str: nome do programa
        self.statements = statements  # list[Statement]



# DECLARAÇÕES
class Declaration(ASTNode):
    """INTEGER/REAL/LOGICAL var1, var2, ..."""

    def __init__(self, type_spec, variables, lineno=None):
        super().__init__(lineno)
        self.type_spec = type_spec  # str: 'INTEGER', 'REAL', 'LOGICAL'
        self.variables = variables  # list[str]: nomes das variáveis


# STATEMENTS
class Assignment(ASTNode):
    """Atribuição: var = expression"""

    def __init__(self, target, value, lineno=None):
        super().__init__(lineno)
        self.target = target  # str: nome da variável
        self.value = value  # Expression


class PrintStatement(ASTNode):
    """PRINT *, expr1, expr2, ..."""

    def __init__(self, expressions, lineno=None):
        super().__init__(lineno)
        self.expressions = expressions  # list[Expression]


class ReadStatement(ASTNode):
    """READ *, var1, var2, ..."""

    def __init__(self, variables, lineno=None):
        super().__init__(lineno)
        self.variables = variables  # list[str]: nomes das variáveis


class IfStatement(ASTNode):
    """IF (cond) THEN ... [ELSE ...] ENDIF"""

    def __init__(self, condition, then_branch, else_branch=None, lineno=None):
        super().__init__(lineno)
        self.condition = condition  # Expression
        self.then_branch = then_branch  # list[Statement]
        self.else_branch = else_branch  # list[Statement] or None


class DoLoop(ASTNode):
    """DO label I = start, end ... label CONTINUE"""

    def __init__(self, label, variable, start, end, body, lineno=None):
        super().__init__(lineno)
        self.label = label  # int: número do label
        self.variable = variable  # str: variável do loop
        self.start = start  # Expression
        self.end = end  # Expression
        self.body = body  # list[Statement]


class ContinueStatement(ASTNode):
    """label CONTINUE"""

    def __init__(self, label, lineno=None):
        super().__init__(lineno)
        self.label = label  # int: número do label


class GotoStatement(ASTNode):
    """GOTO label"""

    def __init__(self, label, lineno=None):
        super().__init__(lineno)
        self.label = label  # int: número do label


# EXPRESSÕES
class BinaryOp(ASTNode):
    """Operação binária: left op right"""

    def __init__(self, left, op, right, lineno=None):
        super().__init__(lineno)
        self.left = left  # Expression
        self.op = op  # str: '+', '-', '*', '/', '.EQ.', '.AND.', etc.
        self.right = right  # Expression


class UnaryOp(ASTNode):
    """Operação unária: op operand"""

    def __init__(self, op, operand, lineno=None):
        super().__init__(lineno)
        self.op = op  # str: '-', '.NOT.'
        self.operand = operand  # Expression


class Variable(ASTNode):
    """Referência a variável: X"""

    def __init__(self, name, lineno=None):
        super().__init__(lineno)
        self.name = name  # str: nome da variável


class IntLiteral(ASTNode):
    """Número inteiro: 42"""

    def __init__(self, value, lineno=None):
        super().__init__(lineno)
        self.value = value  # int


class RealLiteral(ASTNode):
    """Número real: 3.14"""

    def __init__(self, value, lineno=None):
        super().__init__(lineno)
        self.value = value  # float


class StringLiteral(ASTNode):
    """String: 'hello'"""

    def __init__(self, value, lineno=None):
        super().__init__(lineno)
        self.value = value  # str (sem aspas)


class BooleanLiteral(ASTNode):
    """.TRUE. ou .FALSE."""

    def __init__(self, value, lineno=None):
        super().__init__(lineno)
        self.value = value  # bool: True ou False


# UTILITÁRIOS
def print_ast(node, indent=0):
    """
    Imprime AST de forma legível (para debug).

    Args:
        node: Nó AST ou list de nós
        indent: Nível de indentação
    """
    prefix = "  " * indent

    if isinstance(node, list):
        for item in node:
            print_ast(item, indent)
        return

    if not isinstance(node, ASTNode):
        print(f"{prefix}{type(node).__name__}: {repr(node)}")
        return

    # Print do nó atual
    print(f"{prefix}{node.__class__.__name__}")

    # Print dos atributos
    for key, value in node.__dict__.items():
        if key == "lineno":
            continue

        print(f"{prefix}  {key}:")

        if isinstance(value, list):
            for item in value:
                print_ast(item, indent + 2)
        elif isinstance(value, ASTNode):
            print_ast(value, indent + 2)
        elif isinstance(value, str):
            print(f"{prefix}    '{value}'")
        else:
            print(f"{prefix}    {repr(value)}")

class Subroutine(ASTNode):
    def __init__(self, name, params, body, lineno=None):
        super().__init__(lineno)
        self.name = name
        self.params = params
        self.body = body

class Function(ASTNode):
    def __init__(self, type_spec, name, params, body, lineno=None):
        super().__init__(lineno)
        self.type_spec = type_spec
        self.name = name
        self.params = params
        self.body = body

class CallStatement(ASTNode):
    def __init__(self, name, args, lineno=None):
        super().__init__(lineno)
        self.name = name
        self.args = args

class ReturnStatement(ASTNode):
    def __init__(self, lineno=None):
        super().__init__(lineno)

class FunctionCall(ASTNode):
    def __init__(self, name, args, lineno=None):
        super().__init__(lineno)
        self.name = name
        self.args = args
