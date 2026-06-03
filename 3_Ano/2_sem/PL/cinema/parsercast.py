import ply.yacc as yacc
from collections import defaultdict
from lexer import tokens, LexError, lexer
import sys

class ParseError(Exception):
    pass

def p_cinema(p):
    r"cinema : '[' Lista ']'"
    p[0] = p[2]
 
 
def p_lista_multiple(p):
    r"Lista : Lista ',' Filme"
    for c, l in p[1].items():
        p[3].setdefault(c, []).extend(l)
    p[0] = p[3]
 
 
def p_lista_single(p):
    r"Lista : Filme"
    p[0] = p[1]
 
 
def p_filme(p):
    r"Filme : '{' Titulo ',' Ano ',' Elenco ',' Generos '}'"
    p[0] = { c : [p[2]] for c in p[6]}
 
 
def p_titulo(p):
    r"Titulo : TITLE STR"
    p[0] = p[2]
 
 
def p_ano(p):
    r"Ano : YEAR INT"
    p[0] = p[2]
 
 
def p_elenco(p):
    r"Elenco : CAST '[' ListaStr ']'"
    p[0] = p[3]
 
 
def p_generos(p):
    r"Generos : GENRES '[' ListaStr ']'"
    p[0] = p[3]
 
 
def p_listastr_multiple(p):
    r"ListaStr : ListaStr ',' STR"
    p[0] = p[1] + [p[3]]
 
 
def p_listastr_single(p):
    r"ListaStr : STR"
    p[0] = [p[1]]
 
 
def p_error(p):
    if p:
        raise ParseError(f"Unexpected token {p.type!r} ({p.value!r}) at line {p.lineno}")
    raise ParseError("Unexpected end of input")
 
 
parser = yacc.yacc(outputdir="/tmp", debug=False, errorlog=yacc.NullLogger())