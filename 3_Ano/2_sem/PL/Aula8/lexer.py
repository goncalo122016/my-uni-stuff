import ply.lex as lex

states = (('EXPRESSION', 'exclusive'), ('VAR', 'exclusive'))
tokens = ('INT', 'IDEN', 'ADD', 'MUL', 'SUB', 'DIV', 'STAR', 'EQ', 'HASH', 'QUESTION', 'EXCLAMATION')

literals = '()'

def t_STAR(t):
    r'\*'
    return t

def t_HASH(t):
    r'\#'
    return t

def t_QUESTION(t):
    r'\?'
    t.lexer.begin('VAR')
    return t

def t_EXCLAMATION(t):
    r'\!'
    t.lexer.begin('EXPRESSION')
    return t

def t_EQ(t):
    r'='
    t.lexer.begin('EXPRESSION')
    return t

def t_ANY_IDEN(t):
    r'[a-zA-Z_]+'
    return t

def t_EXPRESSION_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

t_EXPRESSION_ADD = r'\+'
t_EXPRESSION_SUB = r'\-'
t_EXPRESSION_MUL = r'\*'
t_EXPRESSION_DIV = r'\/'

t_ANY_ignore = ' \t'

def t_ANY_newline(t):
    r'\n'
    t.lexer.begin('INITIAL')
    t.lexer.lineno += 1

class LexError(Exception):
    pass

def t_ANY_error(t):
    t.lexer.begin('INITIAL')
    raise LexError(f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}")

lexer = lex.lex()