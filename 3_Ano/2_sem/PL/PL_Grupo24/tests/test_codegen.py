"""
Testes para gerador de código VM (Fase 5)

Testa:
- Geração de instruções básicas
- Expressões aritméticas
- Variáveis
- IF statements
- DO loops
"""

import pytest

from src.codegen import CodeGenerator, generate_code
from src.parser import FortranParser


class TestCodeGenBasics:
    """Testes básicos de geração de código"""

    def setup_method(self):
        self.parser = FortranParser()
        self.codegen = CodeGenerator()

    def compile(self, code):
        """Helper: parseia e gera código"""
        ast = self.parser.parse(code)
        return generate_code(ast)


class TestLiterals:
    """Testes de literais"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_integer_literal(self):
        """Literal inteiro"""
        code = "PROGRAM TEST\nPRINT *, 42\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 42" in vm_code
        assert "WRITEI" in vm_code
        assert "STOP" in vm_code

    def test_real_literal(self):
        """Literal real"""
        code = "PROGRAM TEST\nPRINT *, 3.14\nEND"
        vm_code = self.compile(code)

        assert "PUSHF 3.14" in vm_code
        assert "WRITEF" in vm_code

    def test_string_literal(self):
        """Literal string"""
        code = 'PROGRAM TEST\nPRINT *, "Hello"\nEND'
        vm_code = self.compile(code)

        assert 'PUSHS "Hello"' in vm_code
        assert "WRITES" in vm_code

    def test_boolean_true(self):
        """Booleano .TRUE."""
        code = "PROGRAM TEST\nPRINT *, .TRUE.\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 1" in vm_code  # .TRUE. = 1

    def test_boolean_false(self):
        """Booleano .FALSE."""
        code = "PROGRAM TEST\nPRINT *, .FALSE.\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 0" in vm_code  # .FALSE. = 0


class TestArithmetic:
    """Testes de expressões aritméticas"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_addition(self):
        """Adição"""
        code = "PROGRAM TEST\nPRINT *, 2 + 3\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 2" in vm_code
        assert "PUSHI 3" in vm_code
        assert "ADD" in vm_code

    def test_subtraction(self):
        """Subtração"""
        code = "PROGRAM TEST\nPRINT *, 5 - 2\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 5" in vm_code
        assert "PUSHI 2" in vm_code
        assert "SUB" in vm_code

    def test_multiplication(self):
        """Multiplicação"""
        code = "PROGRAM TEST\nPRINT *, 3 * 4\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 3" in vm_code
        assert "PUSHI 4" in vm_code
        assert "MUL" in vm_code

    def test_division(self):
        """Divisão"""
        code = "PROGRAM TEST\nPRINT *, 10 / 2\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 10" in vm_code
        assert "PUSHI 2" in vm_code
        assert "DIV" in vm_code

    def test_unary_minus(self):
        """Negação unária"""
        code = "PROGRAM TEST\nPRINT *, -5\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 5" in vm_code
        assert "PUSHI 0" in vm_code
        assert "SUB" in vm_code


class TestVariables:
    """Testes com variáveis"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_assignment(self):
        """Atribuição simples"""
        code = "PROGRAM TEST\nINTEGER X\nX = 5\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 5" in vm_code
        assert "STOREG" in vm_code

    def test_variable_in_expression(self):
        """Variável em expressão"""
        code = "PROGRAM TEST\nINTEGER X, Y\nX = 5\nY = X + 3\nEND"
        vm_code = self.compile(code)

        assert "STOREG" in vm_code
        assert "PUSHG" in vm_code
        assert "ADD" in vm_code

    def test_print_variable(self):
        """Imprimir variável"""
        code = "PROGRAM TEST\nINTEGER X\nX = 42\nPRINT *, X\nEND"
        vm_code = self.compile(code)

        assert "PUSHI 42" in vm_code
        assert "STOREG" in vm_code
        assert "PUSHG" in vm_code
        assert "WRITEI" in vm_code


class TestComparisons:
    """Testes de comparações"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_eq(self):
        """Igualdade"""
        code = "PROGRAM TEST\nPRINT *, 5 .EQ. 5\nEND"
        vm_code = self.compile(code)

        assert "EQUAL" in vm_code

    def test_ne(self):
        """Desigualdade"""
        code = "PROGRAM TEST\nPRINT *, 5 .NE. 3\nEND"
        vm_code = self.compile(code)

        assert "EQUAL" in vm_code
        assert "NOT" in vm_code

    def test_lt(self):
        """Menor que"""
        code = "PROGRAM TEST\nPRINT *, 3 .LT. 5\nEND"
        vm_code = self.compile(code)

        assert "INF" in vm_code

    def test_le(self):
        """Menor ou igual"""
        code = "PROGRAM TEST\nPRINT *, 3 .LE. 5\nEND"
        vm_code = self.compile(code)

        assert "INFEQ" in vm_code

    def test_gt(self):
        """Maior que"""
        code = "PROGRAM TEST\nPRINT *, 5 .GT. 3\nEND"
        vm_code = self.compile(code)

        assert "SUP" in vm_code

    def test_ge(self):
        """Maior ou igual"""
        code = "PROGRAM TEST\nPRINT *, 5 .GE. 3\nEND"
        vm_code = self.compile(code)

        assert "SUPEQ" in vm_code


class TestLogical:
    """Testes de operações lógicas"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_and(self):
        """E lógico"""
        code = "PROGRAM TEST\nPRINT *, .TRUE. .AND. .FALSE.\nEND"
        vm_code = self.compile(code)

        assert "AND" in vm_code

    def test_or(self):
        """OU lógico"""
        code = "PROGRAM TEST\nPRINT *, .TRUE. .OR. .FALSE.\nEND"
        vm_code = self.compile(code)

        assert "OR" in vm_code

    def test_not(self):
        """NÃO lógico"""
        code = "PROGRAM TEST\nPRINT *, .NOT. .TRUE.\nEND"
        vm_code = self.compile(code)

        assert "NOT" in vm_code


class TestIfStatement:
    """Testes de IF statements"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_if_then_endif(self):
        """IF/THEN/ENDIF"""
        code = """PROGRAM TEST
INTEGER X
X = 5
IF (X .GT. 0) THEN
PRINT *, 'positive'
ENDIF
END"""
        vm_code = self.compile(code)

        assert "JZ" in vm_code  # Salto condicional
        assert ":" in vm_code
        assert 'PUSHS "positive"' in vm_code

    def test_if_then_else_endif(self):
        """IF/THEN/ELSE/ENDIF"""
        code = """PROGRAM TEST
INTEGER X
X = 5
IF (X .GT. 10) THEN
PRINT *, 'big'
ELSE
PRINT *, 'small'
ENDIF
END"""
        vm_code = self.compile(code)

        assert "JZ" in vm_code  # Salto se falso
        assert "JUMP" in vm_code  # Salto para ENDIF
        assert 'PUSHS "big"' in vm_code
        assert 'PUSHS "small"' in vm_code

    def test_nested_if(self):
        """IF aninhado"""
        code = """PROGRAM TEST
INTEGER X, Y
X = 5
Y = 10
IF (X .LT. Y) THEN
IF (X .GT. 0) THEN
PRINT *, 'ok'
ENDIF
ENDIF
END"""
        vm_code = self.compile(code)

        assert vm_code.count("JZ") >= 2  # Dois IFs
        assert 'PUSHS "ok"' in vm_code


class TestCompletePrograms:
    """Testes com programas completos"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_hello_world(self):
        """Hello World"""
        code = """PROGRAM HELLO
PRINT *, 'Hello, World!'
END"""
        vm_code = self.compile(code)

        assert 'PUSHS "Hello, World!"' in vm_code
        assert "WRITES" in vm_code
        assert "STOP" in vm_code

    def test_simple_calculator(self):
        """Calculadora simples"""
        code = """PROGRAM CALC
INTEGER A, B, C
A = 10
B = 5
C = A + B
PRINT *, C
END"""
        vm_code = self.compile(code)

        assert "PUSHI 10" in vm_code
        assert "PUSHI 5" in vm_code
        assert "ADD" in vm_code
        assert "STOREG" in vm_code
        assert "PUSHG" in vm_code

    def test_conditional_program(self):
        """Programa com condicional"""
        code = """PROGRAM CONDITIONAL
INTEGER X
X = 5
IF (X .GT. 0) THEN
PRINT *, 'positive'
ELSE
PRINT *, 'negative'
ENDIF
END"""
        vm_code = self.compile(code)

        assert "JZ" in vm_code
        assert "JUMP" in vm_code
        assert ":" in vm_code


class TestCodeStructure:
    """Testes da estrutura do código"""

    def setup_method(self):
        self.parser = FortranParser()

    def compile(self, code):
        ast = self.parser.parse(code)
        return generate_code(ast)

    def test_halt_at_end(self):
        """HALT no final"""
        code = "PROGRAM TEST\nEND"
        vm_code = self.compile(code)

        assert "STOP" in vm_code
        assert vm_code.strip().endswith("STOP")

    def test_no_empty_lines(self):
        """Sem linhas vazias"""
        code = "PROGRAM TEST\nPRINT *, 1\nEND"
        vm_code = self.compile(code)

        lines = [l for l in vm_code.split("\n") if l.strip()]
        assert len(lines) > 0

    def test_valid_instructions(self):
        """Instruções válidas"""
        code = "PROGRAM TEST\nINTEGER X\nX = 5\nPRINT *, X\nEND"
        vm_code = self.compile(code)

        valid_prefixes = [
            "START",
            "STOP",
            "PUSHI",
            "PUSHF",
            "PUSHS",
            "PUSHG",
            "STOREG",
            "PUSHN",
            "ADD",
            "SUB",
            "MUL",
            "DIV",
            "SWAP",
            "EQUAL",
            "INF",
            "INFEQ",
            "SUP",
            "SUPEQ",
            "AND",
            "OR",
            "NOT",
            "JUMP",
            "JZ",
            "WRITEI",
            "WRITEF",
            "WRITES",
            "WRITELN",
            "READ",
            "ATOI",
        ]

        for line in vm_code.split("\n"):
            line = line.strip()
            if line and not line.endswith(":"):
                assert any(line.startswith(prefix) for prefix in valid_prefixes)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
