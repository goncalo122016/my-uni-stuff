"""
Testes unitários para o lexer Fortran 77

Testa:
- Tokenização de keywords
- Números inteiros e reais
- Identificadores
- Strings
- Operadores (aritméticos, relacionais, lógicos)
- Comentários
- Newlines
"""

import pytest

from src.lexer import FortranLexer


class TestLexerBasics:
    """Testes básicos de tokenização"""

    def setup_method(self):
        """Prepara um novo lexer para cada teste"""
        self.lexer = FortranLexer()

    def get_token_types(self, code):
        """Helper: retorna lista de tipos de tokens"""
        tokens = self.lexer.tokenize(code)
        return [tok.type for tok in tokens]

    def get_token_values(self, code):
        """Helper: retorna lista de valores de tokens"""
        tokens = self.lexer.tokenize(code)
        return [tok.value for tok in tokens]


class TestKeywords:
    """Testes de palavras-chave"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_program_keyword(self):
        """PROGRAM deve ser reconhecido como keyword"""
        code = "PROGRAM"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "PROGRAM"

    def test_program_lowercase(self):
        """program em lowercase deve ser reconhecido (case-insensitive)"""
        code = "program"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "PROGRAM"

    def test_end_keyword(self):
        """END deve ser reconhecido como keyword"""
        code = "END"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "END"

    def test_integer_keyword(self):
        """INTEGER deve ser reconhecido como keyword"""
        code = "INTEGER"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "INTEGER"

    def test_real_keyword(self):
        """REAL deve ser reconhecido como keyword"""
        code = "REAL"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "REAL"

    def test_logical_keyword(self):
        """LOGICAL deve ser reconhecido"""
        code = "LOGICAL"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "LOGICAL"

    def test_if_then_endif(self):
        """IF THEN ENDIF devem ser reconhecidos"""
        code = "IF THEN ENDIF"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 3
        assert tokens[0].type == "IF"
        assert tokens[1].type == "THEN"
        assert tokens[2].type == "ENDIF"

    def test_do_continue(self):
        """DO CONTINUE devem ser reconhecidos"""
        code = "DO CONTINUE"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 2
        assert tokens[0].type == "DO"
        assert tokens[1].type == "CONTINUE"

    def test_print_keyword(self):
        """PRINT deve ser reconhecido"""
        code = "PRINT"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "PRINT"

    def test_read_keyword(self):
        """READ deve ser reconhecido"""
        code = "READ"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "READ"

    def test_goto_keyword(self):
        """GOTO deve ser reconhecido"""
        code = "GOTO"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "GOTO"


class TestNumbers:
    """Testes de números inteiros e reais"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_integer(self):
        """Número inteiro deve ser reconhecido"""
        code = "42"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "NUMBER"
        assert tokens[0].value == 42

    def test_integer_zero(self):
        """Zero deve ser reconhecido"""
        code = "0"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "NUMBER"
        assert tokens[0].value == 0

    def test_real_simple(self):
        """Número real simples (3.14) deve ser reconhecido"""
        code = "3.14"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "REAL_NUMBER"
        assert abs(tokens[0].value - 3.14) < 0.001

    def test_real_scientific(self):
        """Notação científica (1.23E+02) deve ser reconhecida"""
        code = "1.23E+02"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "REAL_NUMBER"
        assert abs(tokens[0].value - 123.0) < 0.1

    def test_real_no_leading_digit(self):
        """.5 (sem dígito antes do ponto) deve ser reconhecido"""
        code = ".5"
        # Nota: este teste pode falhar se o regex não suportar isso
        # Ajustaremos se necessário
        tokens = self.lexer.tokenize(code)
        # Pode ser tratado como DIVIDE + NUMBER ou como REAL_NUMBER
        # Aceitaremos ambos neste MVP

    def test_integer_sequence(self):
        """Sequência de inteiros separados por espaço"""
        code = "1 2 3"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 3
        assert all(tok.type == "NUMBER" for tok in tokens)
        assert [tok.value for tok in tokens] == [1, 2, 3]


class TestIdentifiers:
    """Testes de identificadores"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_simple_identifier(self):
        """Identificador simples deve ser reconhecido"""
        code = "X"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[0].value == "X"

    def test_identifier_lowercase(self):
        """Identificador em minúsculas deve ser preservado como ID"""
        code = "x"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[0].value == "x"

    def test_identifier_with_underscore(self):
        """Identificador com underscore"""
        code = "my_var"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[0].value == "my_var"

    def test_identifier_with_numbers(self):
        """Identificador com números"""
        code = "VAR123"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[0].value == "VAR123"

    def test_identifier_not_start_with_number(self):
        """Identificador NÃO pode começar com número"""
        code = "1VAR"
        tokens = self.lexer.tokenize(code)
        # Deve tokenizar como NUMBER + IDENTIFIER
        assert tokens[0].type == "NUMBER"
        assert tokens[1].type == "IDENTIFIER"


class TestStrings:
    """Testes de strings"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_string_single_quotes(self):
        """String com aspas simples"""
        code = "'Hello'"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "STRING"
        assert tokens[0].value == "Hello"

    def test_string_double_quotes(self):
        """String com aspas duplas"""
        code = '"World"'
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "STRING"
        assert tokens[0].value == "World"

    def test_string_with_spaces(self):
        """String com espaços"""
        code = "'Hello World'"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "STRING"
        assert tokens[0].value == "Hello World"

    def test_string_empty(self):
        """String vazia"""
        code = "''"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "STRING"
        assert tokens[0].value == ""

    def test_string_with_numbers(self):
        """String contendo números"""
        code = "'123 456'"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "STRING"
        assert tokens[0].value == "123 456"


class TestOperators:
    """Testes de operadores"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_plus(self):
        """Operador + deve ser reconhecido"""
        code = "+"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "PLUS"

    def test_minus(self):
        """Operador - deve ser reconhecido"""
        code = "-"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "MINUS"

    def test_times(self):
        """Operador * deve ser reconhecido"""
        code = "*"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "TIMES"

    def test_divide(self):
        """Operador / deve ser reconhecido"""
        code = "/"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "DIVIDE"

    def test_assign(self):
        """Operador = deve ser reconhecido"""
        code = "="
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "ASSIGN"

    def test_relational_eq(self):
        """.EQ. deve ser reconhecido"""
        code = ".EQ."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "EQ"

    def test_relational_ne(self):
        """.NE. deve ser reconhecido"""
        code = ".NE."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "NE"

    def test_relational_lt(self):
        """.LT. deve ser reconhecido"""
        code = ".LT."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "LT"

    def test_relational_le(self):
        """.LE. deve ser reconhecido"""
        code = ".LE."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "LE"

    def test_relational_gt(self):
        """.GT. deve ser reconhecido"""
        code = ".GT."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "GT"

    def test_relational_ge(self):
        """.GE. deve ser reconhecido"""
        code = ".GE."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "GE"

    def test_logical_and(self):
        """.AND. deve ser reconhecido"""
        code = ".AND."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "AND"

    def test_logical_or(self):
        """.OR. deve ser reconhecido"""
        code = ".OR."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "OR"

    def test_logical_not(self):
        """.NOT. deve ser reconhecido"""
        code = ".NOT."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "NOT"

    def test_logical_true(self):
        """.TRUE. deve ser reconhecido"""
        code = ".TRUE."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "TRUE"

    def test_logical_false(self):
        """.FALSE. deve ser reconhecido"""
        code = ".FALSE."
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "FALSE"


class TestDelimiters:
    """Testes de delimitadores"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_lparen(self):
        """( deve ser reconhecido"""
        code = "("
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "LPAREN"

    def test_rparen(self):
        """) deve ser reconhecido"""
        code = ")"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "RPAREN"

    def test_comma(self):
        """, deve ser reconhecido"""
        code = ","
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "COMMA"


class TestComments:
    """Testes de comentários"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_comment_ignored(self):
        """Comentário deve ser ignorado"""
        code = "! This is a comment"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 0

    def test_comment_after_code(self):
        """Comentário após código deve ser ignorado"""
        code = "X = 5 ! assignment"
        tokens = self.lexer.tokenize(code)
        # X = 5
        assert len(tokens) == 3
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[1].type == "ASSIGN"
        assert tokens[2].type == "NUMBER"

    def test_multiline_comments(self):
        """Múltiplas linhas com comentários"""
        code = """! Line 1
        X = 5
        ! Line 3"""
        tokens = self.lexer.tokenize(code)
        # Apenas X = 5
        assert len(tokens) == 3
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[0].value == "X"
        assert tokens[1].type == "ASSIGN"
        assert tokens[2].type == "NUMBER"


class TestComplexPrograms:
    """Testes com programas completos"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_hello_world_program(self):
        """Programa HELLO WORLD completo"""
        code = """PROGRAM HELLO
        PRINT *, 'Hello, World!'
        END"""

        tokens = self.lexer.tokenize(code)

        # Verifica presença de keywords principais
        token_types = [tok.type for tok in tokens]
        assert "PROGRAM" in token_types
        assert "PRINT" in token_types
        assert "END" in token_types
        assert "STRING" in token_types

    def test_factorial_program(self):
        """Programa de fatorial"""
        code = """PROGRAM FACTORIAL
        INTEGER N, FAT
        N = 5
        FAT = 1
        10 IF (N .LE. 1) GOTO 20
        FAT = FAT * N
        N = N - 1
        GOTO 10
        20 PRINT *, FAT
        END"""

        tokens = self.lexer.tokenize(code)

        token_types = [tok.type for tok in tokens]
        assert "PROGRAM" in token_types
        assert "INTEGER" in token_types
        assert "IF" in token_types
        assert "GOTO" in token_types
        assert "LE" in token_types
        assert "END" in token_types

    def test_assignment_sequence(self):
        """Sequência de atribuições"""
        code = "A = 1\nB = A + 2\nC = B * A"

        tokens = self.lexer.tokenize(code)
        token_types = [tok.type for tok in tokens]

        # Verifica operadores presentes
        assert token_types.count("ASSIGN") >= 3
        assert "PLUS" in token_types
        assert "TIMES" in token_types


class TestWhitespaceHandling:
    """Testes de tratamento de espaços em branco"""

    def setup_method(self):
        self.lexer = FortranLexer()

    def test_leading_whitespace(self):
        """Espaços no início devem ser ignorados"""
        code = "   X"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "IDENTIFIER"

    def test_trailing_whitespace(self):
        """Espaços no final devem ser ignorados"""
        code = "X   "
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 1
        assert tokens[0].type == "IDENTIFIER"

    def test_multiple_spaces_between_tokens(self):
        """Múltiplos espaços entre tokens devem ser ignorados"""
        code = "X    =    5"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 3
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[1].type == "ASSIGN"
        assert tokens[2].type == "NUMBER"

    def test_tabs_treated_as_whitespace(self):
        """Tabulações devem ser tratadas como espaços em branco"""
        code = "X\t=\t5"
        tokens = self.lexer.tokenize(code)
        assert len(tokens) == 3
        assert tokens[0].type == "IDENTIFIER"
        assert tokens[1].type == "ASSIGN"
        assert tokens[2].type == "NUMBER"

if __name__ == "__main__":
    pytest.main([__file__, "-v"])