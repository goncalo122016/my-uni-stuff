import ply.yacc as yacc
from lexer import tokens, LexError

def p_comands_multiple(p):
    r"Comands : Comands Comand"
    p[0] = p[1] + p[2]

def p_comands_single(p):
    r"Comands : Comand"
    p[0] = p[1]

def p_comand(p):
    r"Comand : IDEN EQ Expr"
    if p[1] not in p.parser.vars:
        p.parser.vars.append(p[1])
    index = p.parser.vars.index(p[1])
    p[0] = p[3] + [f"STOREG {index}"]

def p_comand_expr(p):
    r"Comand : EXCLAMATION Expr"
    p[0] = p[2] + ["WRITEI"]

def p_comand_question(p):
    r"Comand : QUESTION IDEN"
    if p[2] not in p.parser.vars:
        p.parser.vars.append(p[2])
    index = p.parser.vars.index(p[2])
    p[0] = [f"PUSHS {p.parser.vars[index]}", "WRITES", "READ", "ATOI", f"STOREG {index}"]

def p_comand_star(p):
    r"Comand : STAR"
    p[0] = []
    for i in range(len(p.parser.vars)):
        p[0].extend([f"PUSHS {p.parser.vars[i]}", "WRITES", f"PUSHG {i}", "WRITEI"])

def p_comand_hash(p):
    r"Comand : HASH"
    p[0] = ["STOP"]

def p_expr_add(p):
    r"Expr : Expr ADD Term"
    p[0] = p[1] + p[3] + ["ADD"]

def p_expr_sub(p):
    r"Expr : Expr SUB Term"
    p[0] = p[1] + p[3] + ["SUB"]

def p_expr_term(p):
    r"Expr : Term"
    p[0] = p[1]

def p_term_mul(p):
    r"Term : Term MUL Factor"
    p[0] = p[1] + p[3] + ["MUL"]

def p_term_div(p):
    r"Term : Term DIV Factor"
    p[0] = p[1] + p[3] + ["DIV"]

def p_term_factor(p):
    r"Term : Factor"
    p[0] = p[1]

def p_factor_int(p):
    r"Factor : INT"
    p[0] = [f"PUSHI {p[1]}"]

def p_factor_expr(p):
    r"Factor : '(' Expr ')'"
    p[0] = p[2]

def p_factor_iden(p):
    r"Factor : IDEN"
    if p[1] not in p.parser.vars:
        raise RunTimeError(f"Runtime error: Undefined variable '{p[1]}' at line {p.lexer.lineno}")
    index = p.parser.vars.index(p[1])
    p[0] = [f"PUSHG {index}"]

class ParseError(Exception):
    pass

class RunTimeError(Exception):
    pass

def p_error(p):
    raise ParseError(f"Syntax error at line {p.lexer.lineno}")

def parse(data):
    parser = yacc.yacc(write_tables=False)
    parser.vars = []
    code = parser.parse(data)
    return '\n'.join(code)

example = """
? x
y = x * 2
! x + y
#
"""

if __name__ == "__main__":    
    try:
        print(parse(example))
    except (LexError, ParseError, RunTimeError) as e:
        print(e)