"""
Testes unitários para o Analisador Semântico (Fase 4)

Testa:
- Tabela de símbolos
- Declarações (redeclaração)
- Uso de variáveis (não declaradas)
- Labels (DO, CONTINUE, GOTO)
- Avisos (variáveis não usadas)
"""

import pytest

from src.parser import FortranParser
from src.semantic import SemanticAnalyzer


class TestDeclarations:
    """Testes de declarações"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    def test_single_declaration(self):
        """Declaração simples"""
        code = "PROGRAM TEST\nINTEGER X\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert len(self.analyzer.errors) == 0
        assert "X" in self.analyzer.symbols

    def test_multiple_declarations(self):
        """Múltiplas variáveis em uma declaração"""
        code = "PROGRAM TEST\nINTEGER X, Y, Z\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert "X" in self.analyzer.symbols
        assert "Y" in self.analyzer.symbols
        assert "Z" in self.analyzer.symbols

    def test_multiple_type_declarations(self):
        """Múltiplas declarações de tipo"""
        code = "PROGRAM TEST\nINTEGER X\nREAL Y\nLOGICAL Z\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert self.analyzer.symbols["X"].type_spec == "INTEGER"
        assert self.analyzer.symbols["Y"].type_spec == "REAL"
        assert self.analyzer.symbols["Z"].type_spec == "LOGICAL"

    def test_redeclaration_error(self):
        """Redeclaração deve gerar erro"""
        code = "PROGRAM TEST\nINTEGER X\nINTEGER X\nEND"
        success = self.parse_and_analyze(code)

        assert success is False
        assert len(self.analyzer.errors) >= 1
        assert (
            "múltiplas vezes" in self.analyzer.errors[0].message.lower()
            or "redeclaração" in self.analyzer.errors[0].message.lower()
        )


class TestVariableUsage:
    """Testes de uso de variáveis"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    def test_assignment_declared_variable(self):
        """Atribuição a variável declarada"""
        code = "PROGRAM TEST\nINTEGER X\nX = 5\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert len(self.analyzer.errors) == 0

    def test_use_undeclared_variable_assignment(self):
        """Atribuição a variável não declarada"""
        code = "PROGRAM TEST\nX = 5\nEND"
        success = self.parse_and_analyze(code)

        assert success is False
        assert len(self.analyzer.errors) >= 1
        assert "não declarada" in self.analyzer.errors[0].message

    def test_use_undeclared_variable_expression(self):
        """Uso de variável não declarada em expressão"""
        code = "PROGRAM TEST\nINTEGER Y\nY = X + 5\nEND"
        success = self.parse_and_analyze(code)

        assert success is False
        assert len(self.analyzer.errors) >= 1

    def test_use_in_print(self):
        """Uso de variável em PRINT"""
        code = "PROGRAM TEST\nINTEGER X\nX = 5\nPRINT *, X\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert self.analyzer.symbols["X"].used is True

    def test_use_undeclared_in_print(self):
        """Variável não declarada em PRINT"""
        code = "PROGRAM TEST\nPRINT *, X\nEND"
        success = self.parse_and_analyze(code)

        assert success is False
        assert len(self.analyzer.errors) >= 1

    def test_use_in_read(self):
        """Uso de variável em READ"""
        code = "PROGRAM TEST\nINTEGER X\nREAD *, X\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert self.analyzer.symbols["X"].used is True

    def test_multiple_uses(self):
        """Múltiplos usos da mesma variável"""
        code = """PROGRAM TEST
INTEGER X, Y
X = 1
Y = X + 1
PRINT *, Y
END"""
        success = self.parse_and_analyze(code)

        assert success is True
        assert self.analyzer.symbols["X"].used is True
        assert self.analyzer.symbols["Y"].used is True


class TestUnusedWarnings:
    """Testes de avisos de variáveis não usadas"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    def test_unused_variable_warning(self):
        """Variável declarada mas não usada gera aviso"""
        code = "PROGRAM TEST\nINTEGER X\nEND"
        success = self.parse_and_analyze(code)

        assert success is True  # Sem erros, só avisos
        assert len(self.analyzer.warnings) >= 1
        assert "nunca usada" in self.analyzer.warnings[0].message

    def test_used_variable_no_warning(self):
        """Variável usada não gera aviso"""
        code = "PROGRAM TEST\nINTEGER X\nX = 5\nPRINT *, X\nEND"
        success = self.parse_and_analyze(code)

        assert success is True
        assert len(self.analyzer.warnings) == 0

    def test_multiple_unused(self):
        """Múltiplas variáveis não usadas"""
        code = "PROGRAM TEST\nINTEGER X, Y, Z\nZ = 5\nEND"
        success = self.parse_and_analyze(code)

        # X e Y não usadas
        assert len(self.analyzer.warnings) >= 2


class TestIfStatement:
    """Testes de IF statements"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    def test_if_with_declared_variable(self):
        """IF com variável declarada"""
        code = """PROGRAM TEST
INTEGER X
X = 5
IF (X .GT. 0) THEN
PRINT *, 'positive'
ENDIF
END"""
        success = self.parse_and_analyze(code)

        assert success is True

    def test_if_with_undeclared_variable(self):
        """IF com variável não declarada"""
        code = """PROGRAM TEST
IF (X .GT. 0) THEN
PRINT *, 'positive'
ENDIF
END"""
        success = self.parse_and_analyze(code)

        assert success is False
        assert len(self.analyzer.errors) >= 1

    def test_if_else_with_assignments(self):
        """IF/ELSE com atribuições"""
        code = """PROGRAM TEST
INTEGER X, Y
X = 5
IF (X .GT. 0) THEN
Y = X + 1
ELSE
Y = X - 1
ENDIF
PRINT *, Y
END"""
        success = self.parse_and_analyze(code)

        assert success is True
        assert self.analyzer.symbols["Y"].used is True

    def test_if_with_complex_condition(self):
        """IF com condição complexa"""
        code = """PROGRAM TEST
INTEGER X, Y, Z
X = 5
Y = 10
Z = 0
IF (X .LT. Y .AND. Y .GT. 0) THEN
Z = 1
ENDIF
PRINT *, Z
END"""
        success = self.parse_and_analyze(code)

        assert success is True


class TestComplexPrograms:
    """Testes com programas complexos"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    def test_calculator_program(self):
        """Programa calculadora com IF"""
        code = """PROGRAM CALC
INTEGER A, B, RESULT
A = 10
B = 5
IF (A .GT. B) THEN
RESULT = A - B
ELSE
RESULT = B - A
ENDIF
PRINT *, RESULT
END"""
        success = self.parse_and_analyze(code)

        assert success is True

    def test_program_with_multiple_errors(self):
        """Programa com múltiplos erros"""
        code = """PROGRAM BAD
INTEGER X
Y = 5
PRINT *, Z
END"""
        success = self.parse_and_analyze(code)

        # Y não declarada, Z não declarada
        assert success is False
        assert len(self.analyzer.errors) >= 2

    def test_hello_world(self):
        """Hello World válido"""
        code = """PROGRAM HELLO
PRINT *, 'Hello'
END"""
        success = self.parse_and_analyze(code)

        assert success is True

    def test_simple_io_program(self):
        """Programa simples com I/O"""
        code = """PROGRAM IO
INTEGER X, Y
READ *, X
READ *, Y
PRINT *, X
PRINT *, Y
END"""
        success = self.parse_and_analyze(code)

        assert success is True


class TestSymbolTable:
    """Testes da tabela de símbolos"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def test_get_symbol(self):
        """Recuperar símbolo da tabela"""
        code = "PROGRAM TEST\nINTEGER X\nEND"
        ast = self.parser.parse(code)
        self.analyzer.analyze(ast)

        symbol = self.analyzer.get_symbol("X")
        assert symbol is not None
        assert symbol.name == "X"
        assert symbol.type_spec == "INTEGER"

    def test_is_declared(self):
        """Verificar se variável está declarada"""
        code = "PROGRAM TEST\nINTEGER X\nEND"
        ast = self.parser.parse(code)
        self.analyzer.analyze(ast)

        assert self.analyzer.is_declared("X") is True
        assert self.analyzer.is_declared("Y") is False

    def test_get_type(self):
        """Recuperar tipo de variável"""
        code = "PROGRAM TEST\nINTEGER X\nREAL Y\nEND"
        ast = self.parser.parse(code)
        self.analyzer.analyze(ast)

        assert self.analyzer.get_type("X") == "INTEGER"
        assert self.analyzer.get_type("Y") == "REAL"
        assert self.analyzer.get_type("Z") is None

    def test_symbol_tracking_used_assigned(self):
        """Rastreamento de símbolo (used, assigned)"""
        code = "PROGRAM TEST\nINTEGER X\nX = 5\nPRINT *, X\nEND"
        ast = self.parser.parse(code)
        self.analyzer.analyze(ast)

        symbol = self.analyzer.get_symbol("X")
        assert symbol is not None
        assert symbol.assigned is True
        assert symbol.used is True


class TestNestedExpressions:
    """Testes com expressões aninhadas"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    def test_arithmetic_expression(self):
        """Expressão aritmética com múltiplas variáveis"""
        code = "PROGRAM TEST\nINTEGER X, Y, Z\nZ = X + Y * 2\nEND"
        success = self.parse_and_analyze(code)

        # Análise semântica não checa se foram atribuídas
        assert success is True
        # Mas as variáveis existem e são reconhecidas

    def test_complex_logical_expression(self):
        """Expressão lógica complexa"""
        code = """PROGRAM TEST
INTEGER A, B
LOGICAL FLAG
A = 5
B = 10
FLAG = A .LT. B .AND. .NOT. (A .EQ. 0)
PRINT *, FLAG
END"""
        success = self.parse_and_analyze(code)

        assert success is True


class TestTypeChecking:
    """Testes de verificação de tipos em expressões e atribuições"""

    def setup_method(self):
        self.parser = FortranParser()
        self.analyzer = SemanticAnalyzer()

    def parse_and_analyze(self, code):
        ast = self.parser.parse(code)
        success = self.analyzer.analyze(ast)
        return success

    # --- Operações aritméticas ---

    def test_logical_in_arithmetic_error(self):
        """LOGICAL em contexto aritmético deve gerar erro"""
        code = """PROGRAM TEST
LOGICAL FLAG
INTEGER X
FLAG = .TRUE.
X = FLAG + 1
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any("LOGICAL" in e.message and "aritmética" in e.message
                   for e in self.analyzer.errors)

    def test_integer_real_mix_warning(self):
        """Mistura INTEGER e REAL deve gerar aviso"""
        code = """PROGRAM TEST
INTEGER X
REAL Y
REAL Z
X = 2
Y = 3.14
Z = X + Y
END"""
        success = self.parse_and_analyze(code)
        assert success is True  # aviso, não erro
        assert any("INTEGER" in w.message and "REAL" in w.message
                   for w in self.analyzer.warnings)

    def test_pure_integer_arithmetic_ok(self):
        """Aritmética puramente INTEGER é válida"""
        code = """PROGRAM TEST
INTEGER A, B, C
A = 1
B = 2
C = A + B * 3
END"""
        success = self.parse_and_analyze(code)
        assert success is True
        assert not any("aritmética" in e.message for e in self.analyzer.errors)

    def test_pure_real_arithmetic_ok(self):
        """Aritmética puramente REAL é válida"""
        code = """PROGRAM TEST
REAL X, Y, Z
X = 1.0
Y = 2.5
Z = X * Y
END"""
        success = self.parse_and_analyze(code)
        assert success is True

    # --- Operações lógicas ---

    def test_integer_in_and_error(self):
        """INTEGER em .AND. deve gerar erro"""
        code = """PROGRAM TEST
INTEGER A, B
LOGICAL R
A = 1
B = 0
R = A .AND. B
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any(".AND." in e.message for e in self.analyzer.errors)

    def test_integer_in_or_error(self):
        """INTEGER em .OR. deve gerar erro"""
        code = """PROGRAM TEST
INTEGER A
LOGICAL R
A = 1
R = A .OR. .TRUE.
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any(".OR." in e.message for e in self.analyzer.errors)

    def test_logical_and_logical_ok(self):
        """LOGICAL .AND. LOGICAL é válido"""
        code = """PROGRAM TEST
LOGICAL A, B, C
A = .TRUE.
B = .FALSE.
C = A .AND. B
END"""
        success = self.parse_and_analyze(code)
        assert success is True

    # --- Operação .NOT. ---

    def test_not_integer_error(self):
        """.NOT. aplicado a INTEGER deve gerar erro"""
        code = """PROGRAM TEST
INTEGER X
LOGICAL R
X = 5
R = .NOT. X
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any(".NOT." in e.message for e in self.analyzer.errors)

    def test_not_logical_ok(self):
        """.NOT. aplicado a LOGICAL é válido"""
        code = """PROGRAM TEST
LOGICAL FLAG, R
FLAG = .TRUE.
R = .NOT. FLAG
END"""
        success = self.parse_and_analyze(code)
        assert success is True

    # --- Negação unária ---

    def test_negate_logical_error(self):
        """Negação aritmética de LOGICAL deve gerar erro"""
        code = """PROGRAM TEST
LOGICAL FLAG
INTEGER X
FLAG = .TRUE.
X = -FLAG
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any("LOGICAL" in e.message and "negado" in e.message
                   for e in self.analyzer.errors)

    # --- Comparações de ordem ---

    def test_logical_in_lt_error(self):
        """LOGICAL em .LT. deve gerar erro"""
        code = """PROGRAM TEST
LOGICAL A, B
LOGICAL R
A = .TRUE.
B = .FALSE.
R = A .LT. B
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any(".LT." in e.message for e in self.analyzer.errors)

    def test_logical_in_ge_error(self):
        """LOGICAL em .GE. deve gerar erro"""
        code = """PROGRAM TEST
LOGICAL A, B
LOGICAL R
A = .TRUE.
B = .FALSE.
R = A .GE. B
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any(".GE." in e.message for e in self.analyzer.errors)

    def test_integer_comparison_ok(self):
        """Comparação de ordem entre INTEGER é válida"""
        code = """PROGRAM TEST
INTEGER X, Y
LOGICAL R
X = 3
Y = 5
R = X .LT. Y
END"""
        success = self.parse_and_analyze(code)
        assert success is True

    # --- Tipo em atribuições ---

    def test_assign_logical_to_integer_error(self):
        """Atribuir LOGICAL a INTEGER deve gerar erro"""
        code = """PROGRAM TEST
INTEGER X
X = .TRUE.
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any("LOGICAL" in e.message and "INTEGER" in e.message
                   for e in self.analyzer.errors)

    def test_assign_logical_to_real_error(self):
        """Atribuir LOGICAL a REAL deve gerar erro"""
        code = """PROGRAM TEST
REAL X
X = .FALSE.
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any("LOGICAL" in e.message and "REAL" in e.message
                   for e in self.analyzer.errors)

    def test_assign_real_to_logical_error(self):
        """Atribuir REAL a LOGICAL deve gerar erro"""
        code = """PROGRAM TEST
LOGICAL FLAG
FLAG = 3.14
END"""
        success = self.parse_and_analyze(code)
        assert success is False
        assert any("REAL" in e.message and "LOGICAL" in e.message
                   for e in self.analyzer.errors)

    def test_assign_real_to_integer_warning(self):
        """Atribuir REAL a INTEGER deve gerar aviso de truncamento"""
        code = """PROGRAM TEST
INTEGER X
X = 3.14
END"""
        success = self.parse_and_analyze(code)
        assert success is True  # aviso, não erro
        assert any("truncamento" in w.message for w in self.analyzer.warnings)

    def test_assign_integer_to_real_ok(self):
        """Atribuir INTEGER a REAL é válido"""
        code = """PROGRAM TEST
REAL X
X = 5
END"""
        success = self.parse_and_analyze(code)
        assert success is True
        assert not any("truncamento" in w.message for w in self.analyzer.warnings)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
