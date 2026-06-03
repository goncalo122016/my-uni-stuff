"""
Testes unitários para o Parser (Fase 3)

Testa:
- Parsing de programa completo
- Declarações
- Atribuições
- Expressões (precedência)
- If statements
- Do loops
- Print e Read
"""

import pytest

from src.ast_nodes import (
    Assignment,
    BinaryOp,
    BooleanLiteral,
    Declaration,
    IfStatement,
    IntLiteral,
    PrintStatement,
    Program,
    ReadStatement,
    RealLiteral,
    StringLiteral,
    UnaryOp,
    Variable,
)
from src.parser import FortranParser


class TestParserBasics:
    """Testes básicos do parser"""

    def setup_method(self):
        """Setup antes de cada teste"""
        self.parser = FortranParser()

    def parse(self, code):
        """Helper para parsear código"""
        return self.parser.parse(code)


class TestMinimalProgram:
    """Testes de programa mínimo"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_empty_program(self):
        """PROGRAM ... END mínimo"""
        code = "PROGRAM TEST\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        assert isinstance(ast, Program)
        assert ast.name == "TEST"
        assert len(ast.statements) == 0

    def test_program_with_newlines(self):
        """PROGRAM com newlines"""
        code = "PROGRAM TEST\n\nEND\n"
        ast = self.parser.parse(code)

        assert ast is not None
        assert isinstance(ast, Program)


class TestDeclarations:
    """Testes de declarações"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_integer_declaration_single(self):
        """INTEGER X"""
        code = "PROGRAM TEST\nINTEGER X\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        assert len(ast.statements) == 1
        stmt = ast.statements[0]
        assert isinstance(stmt, Declaration)
        assert stmt.type_spec == "INTEGER"
        assert stmt.variables == ["X"]

    def test_integer_declaration_multiple(self):
        """INTEGER X, Y, Z"""
        code = "PROGRAM TEST\nINTEGER X, Y, Z\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        assert len(ast.statements) == 1
        stmt = ast.statements[0]
        assert isinstance(stmt, Declaration)
        assert stmt.variables == ["X", "Y", "Z"]

    def test_real_declaration(self):
        """REAL A, B"""
        code = "PROGRAM TEST\nREAL A, B\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        stmt = ast.statements[0]
        assert isinstance(stmt, Declaration)
        assert stmt.type_spec == "REAL"
        assert stmt.variables == ["A", "B"]

    def test_logical_declaration(self):
        """LOGICAL FLAG"""
        code = "PROGRAM TEST\nLOGICAL FLAG\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        stmt = ast.statements[0]
        assert isinstance(stmt, Declaration)
        assert stmt.type_spec == "LOGICAL"
        assert stmt.variables == ["FLAG"]


class TestAssignments:
    """Testes de atribuição"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_simple_integer_assignment(self):
        """X = 42"""
        code = "PROGRAM TEST\nX = 42\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        stmt = ast.statements[0]
        assert isinstance(stmt, Assignment)
        assert stmt.target == "X"
        assert isinstance(stmt.value, IntLiteral)
        assert stmt.value.value == 42

    def test_simple_real_assignment(self):
        """Y = 3.14"""
        code = "PROGRAM TEST\nY = 3.14\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        stmt = ast.statements[0]
        assert isinstance(stmt, Assignment)
        assert stmt.target == "Y"
        assert isinstance(stmt.value, RealLiteral)
        assert abs(stmt.value.value - 3.14) < 0.001

    def test_variable_assignment(self):
        """X = Y"""
        code = "PROGRAM TEST\nX = Y\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        stmt = ast.statements[0]
        assert isinstance(stmt, Assignment)
        assert stmt.target == "X"
        assert isinstance(stmt.value, Variable)
        assert stmt.value.name == "Y"


class TestExpressions:
    """Testes de expressões"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_addition(self):
        """X = 1 + 2"""
        code = "PROGRAM TEST\nX = 1 + 2\nEND"
        ast = self.parser.parse(code)

        assert ast is not None
        stmt = ast.statements[0]
        assert isinstance(stmt, Assignment)
        expr = stmt.value
        assert isinstance(expr, BinaryOp)
        assert expr.op == "+"

    def test_subtraction(self):
        """X = 5 - 3"""
        code = "PROGRAM TEST\nX = 5 - 3\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == "-"

    def test_multiplication(self):
        """X = 3 * 4"""
        code = "PROGRAM TEST\nX = 3 * 4\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == "*"

    def test_division(self):
        """X = 10 / 2"""
        code = "PROGRAM TEST\nX = 10 / 2\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == "/"

    def test_precedence_multiply_before_add(self):
        """X = 1 + 2 * 3 deve ser 1 + (2 * 3)"""
        code = "PROGRAM TEST\nX = 1 + 2 * 3\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        # Deve ser +
        assert isinstance(expr, BinaryOp)
        assert expr.op == "+"
        # Lado direito deve ser * (multiplicação tem precedência)
        assert isinstance(expr.right, BinaryOp)
        assert expr.right.op == "*"

    def test_parentheses(self):
        """X = (1 + 2) * 3"""
        code = "PROGRAM TEST\nX = (1 + 2) * 3\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        # Deve ser *
        assert isinstance(expr, BinaryOp)
        assert expr.op == "*"
        # Lado esquerdo deve ser + (dentro de parênteses)
        assert isinstance(expr.left, BinaryOp)
        assert expr.left.op == "+"

    def test_unary_minus(self):
        """X = -5"""
        code = "PROGRAM TEST\nX = -5\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, UnaryOp)
        assert expr.op == "-"

    def test_relational_eq(self):
        """X = A .EQ. B"""
        code = "PROGRAM TEST\nX = A .EQ. B\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == ".EQ."

    def test_relational_lt(self):
        """X = A .LT. B"""
        code = "PROGRAM TEST\nX = A .LT. B\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == ".LT."

    def test_logical_and(self):
        """X = A .AND. B"""
        code = "PROGRAM TEST\nX = A .AND. B\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == ".AND."

    def test_logical_or(self):
        """X = A .OR. B"""
        code = "PROGRAM TEST\nX = A .OR. B\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BinaryOp)
        assert expr.op == ".OR."

    def test_logical_not(self):
        """X = .NOT. A"""
        code = "PROGRAM TEST\nX = .NOT. A\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, UnaryOp)
        assert expr.op == ".NOT."


class TestLiterals:
    """Testes de literais"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_integer_literal(self):
        """Literal inteiro 42"""
        code = "PROGRAM TEST\nX = 42\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, IntLiteral)
        assert expr.value == 42

    def test_real_literal(self):
        """Literal real 3.14"""
        code = "PROGRAM TEST\nX = 3.14\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, RealLiteral)
        assert abs(expr.value - 3.14) < 0.001

    def test_string_literal(self):
        """Literal string 'hello'"""
        code = "PROGRAM TEST\nPRINT *, 'hello'\nEND"
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, PrintStatement)
        assert len(stmt.expressions) == 1
        expr = stmt.expressions[0]
        assert isinstance(expr, StringLiteral)
        assert expr.value == "hello"

    def test_boolean_true(self):
        """.TRUE."""
        code = "PROGRAM TEST\nX = .TRUE.\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BooleanLiteral)
        assert expr.value is True

    def test_boolean_false(self):
        """.FALSE."""
        code = "PROGRAM TEST\nX = .FALSE.\nEND"
        ast = self.parser.parse(code)

        expr = ast.statements[0].value
        assert isinstance(expr, BooleanLiteral)
        assert expr.value is False


class TestPrintStatement:
    """Testes de PRINT"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_print_single_expression(self):
        """PRINT *, X"""
        code = "PROGRAM TEST\nPRINT *, X\nEND"
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, PrintStatement)
        assert len(stmt.expressions) == 1

    def test_print_multiple_expressions(self):
        """PRINT *, X, Y, Z"""
        code = "PROGRAM TEST\nPRINT *, X, Y, Z\nEND"
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, PrintStatement)
        assert len(stmt.expressions) == 3

    def test_print_with_literals(self):
        """PRINT *, 'Result:', X"""
        code = "PROGRAM TEST\nPRINT *, 'Result:', X\nEND"
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, PrintStatement)
        assert len(stmt.expressions) == 2
        assert isinstance(stmt.expressions[0], StringLiteral)


class TestReadStatement:
    """Testes de READ"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_read_single_variable(self):
        """READ *, X"""
        code = "PROGRAM TEST\nREAD *, X\nEND"
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, ReadStatement)
        assert stmt.variables == ["X"]

    def test_read_multiple_variables(self):
        """READ *, X, Y, Z"""
        code = "PROGRAM TEST\nREAD *, X, Y, Z\nEND"
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, ReadStatement)
        assert stmt.variables == ["X", "Y", "Z"]


class TestIfStatement:
    """Testes de IF"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_if_then_endif(self):
        """IF (...) THEN ... ENDIF"""
        code = """PROGRAM TEST
IF (X .GT. 0) THEN
X = X + 1
ENDIF
END"""
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, IfStatement)
        assert stmt.condition is not None
        assert len(stmt.then_branch) > 0
        assert stmt.else_branch is None

    def test_if_then_else_endif(self):
        """IF (...) THEN ... ELSE ... ENDIF"""
        code = """PROGRAM TEST
IF (X .GT. 0) THEN
X = X + 1
ELSE
X = X - 1
ENDIF
END"""
        ast = self.parser.parse(code)

        stmt = ast.statements[0]
        assert isinstance(stmt, IfStatement)
        assert stmt.then_branch is not None
        assert stmt.else_branch is not None
        assert len(stmt.then_branch) > 0
        assert len(stmt.else_branch) > 0


class TestComplexPrograms:
    """Testes com programas completos"""

    def setup_method(self):
        self.parser = FortranParser()

    def test_hello_world_parsing(self):
        """Hello World completo"""
        code = """PROGRAM HELLO
PRINT *, 'Hello World'
END"""
        ast = self.parser.parse(code)

        assert ast is not None
        assert isinstance(ast, Program)
        assert ast.name == "HELLO"
        assert len(ast.statements) == 1
        assert isinstance(ast.statements[0], PrintStatement)

    def test_simple_calculation(self):
        """Programa com declarações e cálculos"""
        code = """PROGRAM CALC
INTEGER X, Y, Z
X = 5
Y = 3
Z = X + Y
PRINT *, Z
END"""
        ast = self.parser.parse(code)

        assert ast is not None
        assert len(ast.statements) == 5

    def test_program_with_if(self):
        """Programa com IF"""
        code = """PROGRAM TEST
INTEGER X
X = 10
IF (X .GT. 5) THEN
PRINT *, 'Greater'
ENDIF
END"""
        ast = self.parser.parse(code)

        assert ast is not None
        # Declaration, Assignment, IfStatement
        if_stmt = next((s for s in ast.statements if isinstance(s, IfStatement)), None)
        assert if_stmt is not None


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
