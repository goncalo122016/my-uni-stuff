"""
Lexer para Fortran 77 (free-form simplificado)
Utiliza PLY (ply.lex) para tokenização.

Decisões de design:
- Free-form: sem limite de 72 caracteres
- Case-insensitive para keywords
- Labels: números inteiros no início de statements
- Comentários: ! até fim de linha
- Strings: ' ou "
"""

import re

import ply.lex as lex


class LexerError(Exception):
    """Exceção para erros de tokenização."""

    def __init__(self, message, lineno, lexpos=None):
        super().__init__(message)
        self.lineno = lineno
        self.lexpos = lexpos

    def __str__(self):
        return f"[Linha {self.lineno}] {super().__str__()}"


class FortranLexer:
    """Lexer para Fortran 77 em free-form."""

    # Lista de tokens reconhecidos
    tokens = (
        # Literais
        "NUMBER",
        "REAL_NUMBER",
        "STRING",
        "IDENTIFIER",
        # Keywords
        "PROGRAM",
        "END",
        "INTEGER",
        "REAL",
        "LOGICAL",
        "IF",
        "THEN",
        "ELSE",
        "ENDIF",
        "DO",
        "CONTINUE",
        "GOTO",
        "PRINT",
        "READ",
        "TRUE",
        "FALSE",
        # Operadores aritméticos
        "PLUS",
        "MINUS",
        "TIMES",
        "DIVIDE",
        # Parênteses e delimitadores
        "LPAREN",
        "RPAREN",
        "COMMA",
        # Atribuição
        "ASSIGN",
        # Operadores relacionais (Fortran)
        "EQ",
        "NE",
        "LT",
        "LE",
        "GT",
        "GE",
        # Operadores lógicos (Fortran)
        "AND",
        "OR",
        "NOT",
        "SUBROUTINE",
        "FUNCTION",
        "CALL",
        "RETURN",
    )

    # Palavras-chave reservadas (case-insensitive)
    reserved = {
        "program": "PROGRAM",
        "end": "END",
        "integer": "INTEGER",
        "real": "REAL",
        "logical": "LOGICAL",
        "if": "IF",
        "then": "THEN",
        "else": "ELSE",
        "endif": "ENDIF",
        "do": "DO",
        "continue": "CONTINUE",
        "goto": "GOTO",
        "print": "PRINT",
        "read": "READ",
        "true": "TRUE",
        "false": "FALSE",
        "subroutine": "SUBROUTINE",
        "function": "FUNCTION",
        "call": "CALL",
        "return": "RETURN",
    }

    def __init__(self):
        """Inicializa o lexer."""
        self.lexer = None
        self.build()

    def build(self, **kwargs):
        """Constrói o lexer com ply.lex."""
        self.lexer = lex.lex(module=self, **kwargs)

    # Estados para rastreamento
    def t_newline(self, t):
        r"\n+"
        t.lexer.lineno += len(t.value)
        pass

    # Comentários: ! até fim de linha
    def t_COMMENT(self, t):
        r"!.*"
        # Comentários são ignorados
        pass

    # Strings com aspas simples
    def t_STRING_SINGLE(self, t):
        r"'([^'\\]|\\.)*'"
        t.type = "STRING"
        # Remove aspas externas
        t.value = t.value[1:-1]
        return t

    # Strings com aspas duplas
    def t_STRING_DOUBLE(self, t):
        r'"([^"\\]|\\.)*"'
        t.type = "STRING"
        # Remove aspas externas
        t.value = t.value[1:-1]
        return t

    # Números reais (com ponto decimal)
    def t_REAL_NUMBER(self, t):
        r"(\d+\.\d*|\.\d+)([eE][+-]?\d+)?|\d+[eE][+-]?\d+"
        t.value = float(t.value)
        return t

    # Números inteiros
    def t_NUMBER(self, t):
        r"\d+"
        t.value = int(t.value)
        return t

    # Operadores relacionais Fortran (multi-char com pontos)
    def t_EQ(self, t):
        r"\.[Ee][Qq]\."
        return t

    def t_NE(self, t):
        r"\.[Nn][Ee]\."
        return t

    def t_LT(self, t):
        r"\.[Ll][Tt]\."
        return t

    def t_LE(self, t):
        r"\.[Ll][Ee]\."
        return t

    def t_GT(self, t):
        r"\.[Gg][Tt]\."
        return t

    def t_GE(self, t):
        r"\.[Gg][Ee]\."
        return t

    # Operadores lógicos Fortran
    def t_AND(self, t):
        r"\.[Aa][Nn][Dd]\."
        return t

    def t_OR(self, t):
        r"\.[Oo][Rr]\."
        return t

    def t_NOT(self, t):
        r"\.[Nn][Oo][Tt]\."
        return t

    # Operadores aritméticos de char único
    def t_PLUS(self, t):
        r"\+"
        return t

    def t_MINUS(self, t):
        r"-"
        return t

    def t_TIMES(self, t):
        r"\*"
        t.type = "TIMES"
        return t

    def t_DIVIDE(self, t):
        r"/"
        return t

    # Parênteses e delimitadores
    def t_LPAREN(self, t):
        r"\("
        return t

    def t_RPAREN(self, t):
        r"\)"
        return t

    def t_COMMA(self, t):
        r","
        return t

    # Atribuição
    def t_ASSIGN(self, t):
        r"="
        return t

    def t_TRUE(self, t):
        r"\.TRUE\.|\.true\."
        t.value = True
        return t

    def t_FALSE(self, t):
        r"\.FALSE\.|\.false\."
        t.value = False
        return t

    # Identificadores e keywords
    def t_IDENTIFIER(self, t):
        r"[a-zA-Z][a-zA-Z0-9_]*"
        # Verifica se é keyword (case-insensitive)
        t.type = self.reserved.get(t.value.lower(), "IDENTIFIER")
        return t

    # Erro: caractere inválido
    def t_error(self, t):
        raise LexerError(
            f"Token ilegal '{t.value[0]}'",
            lineno=t.lexer.lineno,
            lexpos=t.lexpos,
        )

    # Espaços em branco (ignorados)
    t_ignore = " \t\r"

    def tokenize(self, data):
        self.lexer.input(data)
        tokens = []

        while True:
            tok = self.lexer.token()
            if not tok:
                break
            tokens.append(tok)

        return tokens

    def test(self, data):
        try:
            tokens = self.tokenize(data)
            for tok in tokens:
                print(f"{tok.type:15} {str(tok.value):20} linha {tok.lineno}")
        except LexerError as e:
            print(f"Erro de lexer: {e}")


def create_lexer():
    """Factory function para criar um novo lexer."""
    return FortranLexer()


if __name__ == "__main__":
    # Teste rápido
    lexer = FortranLexer()

    code = """
    PROGRAM Hello
        INTEGER X
        X = 42
        PRINT *, X
    END
    """

    print("=== Tokenização ===")
    lexer.test(code)
