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
    p[1].append(p[3])
    p[0] = p[1]
 
 
def p_lista_single(p):
    r"Lista : Filme"
    p[0] = [p[1]]
 
 
def p_filme(p):
    r"Filme : '{' Titulo ',' Ano ',' Elenco ',' Generos '}'"
    p[0] = { 'title': p[2], 'year': p[4], 'cast': p[6], 'genres': p[8] }

 
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
 
def cast(movies: list) -> dict:
    result = defaultdict(list)
    for movie in movies:
        for actor in movie["cast"]:
            result[actor].append(movie["title"])
    return dict(sorted(result.items()))

def db(movies: list) -> list:
    result = []
    for movie in movies:
        result.append(f'{{"title": "{movie["title"]}", "year": {movie["year"]}, "cast": {movie["cast"]}, "genres": {movie["genres"]}}}')
    return result

if __name__ == "__main__":
    filename = sys.argv[1] if len(sys.argv) > 1 else "cinema.json"
 
    with open(filename, "r", encoding="utf-8") as fh:
        source = fh.read()
 
    try:
        movies = parser.parse(source, lexer=lexer)
        mapping = cast(movies)
        database = db(movies)
 
        print({movies['Emily Blunt']})
    except ParseError as e:
        print(f"Parse error: {e}")
    except LexError as e:
        print(f"Lexical error: {e}")