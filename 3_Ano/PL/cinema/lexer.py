import ply.lex as lex
import sys
 
class LexError(Exception):
    pass

tokens = (
    "TITLE",
    "YEAR",
    "CAST",
    "GENRES",
    "INT",
    "STR",
)

literals = ("{", "}", "[", "]", ",")
 
t_ignore = " \t\n"
 
 
def t_TITLE(t):
    r'"title":'
    return t
 
 
def t_YEAR(t):
    r'"year":'
    return t
 
 
def t_CAST(t):
    r'"cast":'
    return t
 
 
def t_GENRES(t):
    r'"genres":'
    return t
 
def t_INT(t):
    r"\d+"
    t.value = int(t.value)
    return t
 
 
def t_STR(t):
    r'"(\\"|[^"])*"'
    t.value = t.value[1:-1]
    return t
 
 
def t_error(t):
    raise LexError(f"Illegal character {t.value[0]!r}", t.lineno, t.lexpos)

lexer = lex.lex()

if __name__ == "__main__":
    filename = sys.argv[1] if len(sys.argv) > 1 else "cinema.json"
 
    with open(filename, "r", encoding="utf-8") as fh:
        source = fh.read()
 
    lexer = lex.lex()
    lexer.input(source)

    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)