"""
Testes de integração (Fase 5+)

Testa pipeline completo: Lexer → Parser → Semantic → CodeGen
"""

from pathlib import Path

import pytest

from src.codegen import generate_code
from src.lexer import FortranLexer
from src.parser import FortranParser
from src.semantic import SemanticAnalyzer


class TestIntegration:
    """Testes de integração do pipeline completo"""

    def setup_method(self):
        self.parser = FortranParser()
        self.semantic_analyzer = SemanticAnalyzer()

    def compile_program(self, code):
        """Compila programa do início ao fim"""
        # Lexer
        lexer = FortranLexer()
        tokens = lexer.tokenize(code)

        # Parser
        ast = self.parser.parse(code)
        if ast is None:
            return None, None, self.semantic_analyzer, "Parser error"

        # Semantic analysis
        success = self.semantic_analyzer.analyze(ast)
        if not success:
            return ast, None, self.semantic_analyzer, "Semantic errors"

        # Code generation
        vm_code = generate_code(ast, self.semantic_analyzer)

        return ast, vm_code, self.semantic_analyzer, None

    def test_hello_world_pipeline(self):
        """Hello World através do pipeline completo"""
        code = """PROGRAM HELLO
PRINT *, 'Hello, World!'
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error is None
        assert ast is not None
        assert vm_code is not None
        assert 'PUSHS "Hello, World!"' in vm_code
        assert "WRITES" in vm_code
        assert "STOP" in vm_code

    def test_arithmetic_pipeline(self):
        """Programa aritmético através do pipeline"""
        code = """PROGRAM CALC
INTEGER A, B, C
A = 10
B = 5
C = A + B
PRINT *, C
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error is None
        assert analyzer is not None
        assert vm_code is not None
        assert analyzer.is_declared("A")
        assert analyzer.is_declared("B")
        assert analyzer.is_declared("C")
        assert "ADD" in vm_code
        assert "STOREG" in vm_code

    def test_if_statement_pipeline(self):
        """IF statement através do pipeline"""
        code = """PROGRAM TEST
INTEGER X
X = 5
IF (X .GT. 0) THEN
PRINT *, 'positive'
ELSE
PRINT *, 'negative'
ENDIF
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error is None
        assert vm_code is not None
        assert "JZ" in vm_code
        assert "JUMP" in vm_code
        assert ":" in vm_code

    def test_semantic_error_stops_pipeline(self):
        """Erro semântico pára pipeline"""
        code = """PROGRAM BAD
PRINT *, UNDEFINED
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error == "Semantic errors"
        assert analyzer is not None
        assert len(analyzer.errors) > 0

    def test_multiple_statements_pipeline(self):
        """Múltiplos statements através do pipeline"""
        code = """PROGRAM MULTI
INTEGER X, Y
X = 1
Y = 2
PRINT *, X
PRINT *, Y
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error is None
        assert vm_code is not None
        assert vm_code.count("WRITEI") >= 2
        assert "STOREG" in vm_code

    def test_comparison_pipeline(self):
        """Comparações através do pipeline"""
        code = """PROGRAM COMPARE
PRINT *, 5 .GT. 3
PRINT *, 5 .LT. 3
PRINT *, 5 .EQ. 5
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error is None
        assert vm_code is not None
        assert "SUP" in vm_code
        assert "INF" in vm_code
        assert "EQUAL" in vm_code

    def test_nested_expressions_pipeline(self):
        """Expressões aninhadas através do pipeline"""
        code = """PROGRAM NESTED
INTEGER X, Y, Z
X = 10
Y = 20
Z = X + Y * 2
END"""

        ast, vm_code, analyzer, error = self.compile_program(code)

        assert error is None
        assert analyzer is not None
        assert vm_code is not None
        assert analyzer.is_declared("X")
        assert analyzer.is_declared("Y")
        assert analyzer.is_declared("Z")
        assert "ADD" in vm_code
        assert "MUL" in vm_code


class TestFileGeneration:
    """Testes de geração de ficheiros"""

    def test_vm_file_extension(self):
        """Ficheiro .vm é gerado corretamente"""
        vm_path = Path("tests/results/hello.vm")

        assert vm_path.exists()
        assert vm_path.suffix == ".vm"

    def test_vm_file_contains_stop(self):
        """Ficheiro .vm contém STOP"""
        vm_path = Path("tests/results/hello.vm")

        with open(vm_path) as f:
            content = f.read()

        assert "STOP" in content

    def test_valid_vm_syntax(self):
        """Código VM tem sintaxe válida"""
        vm_path = Path("tests/results/hello.vm")

        with open(vm_path) as f:
            lines = f.readlines()

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

        for line in lines:
            line = line.strip()
            if line and not line.endswith(":"):
                assert any(line.startswith(prefix) for prefix in valid_prefixes)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
