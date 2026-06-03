"""
Parser para Fortran 77 usando PLY (ply.yacc)

Implementa gramática para subconjunto MVP de Fortran 77.
Produz AST explícita.

Precedência de operadores (do menor para o maior):
1. .OR.
2. .AND.
3. .NOT.
4. .EQ., .NE., .LT., .LE., .GT., .GE.
5. +, -
6. *, /
7. Unário: -, .NOT.
8. Parênteses, literais, variáveis
"""

import ply.yacc as yacc

from src.ast_nodes import (
    Assignment,
    BinaryOp,
    BooleanLiteral,
    CallStatement,
    ContinueStatement,
    Declaration,
    DoLoop,
    Function,
    FunctionCall,
    GotoStatement,
    IfStatement,
    IntLiteral,
    PrintStatement,
    Program,
    ReadStatement,
    RealLiteral,
    ReturnStatement,
    StringLiteral,
    Subroutine,
    UnaryOp,
    Variable,
)
from src.lexer import FortranLexer


class ParserError(Exception):
    def __init__(self, message, lineno=None, value=None):
        super().__init__(message)
        self.lineno = lineno
        self.value = value

    def __str__(self):
        if self.lineno:
            return f"[Linha {self.lineno}] {super().__str__()}"
        return super().__str__()


class FortranParser:
    """Parser para Fortran 77"""

    def __init__(self):
        """Inicializa parser com lexer"""
        self.lexer = FortranLexer()
        self.tokens = self.lexer.tokens
        self.parser = None
        self.build()

    def build(self, **kwargs):
        """Constrói o parser com ply.yacc"""
        self.parser = yacc.yacc(
            module=self,
            write_tables=True,  # importante (gera parser.out)
            debug=True,  # ativa debug
            **kwargs,
        )

    precedence = (
        ("left", "OR"),
        ("left", "AND"),
        ("right", "NOT"),
        ("left", "EQ", "NE", "LT", "LE", "GT", "GE"),
        ("left", "PLUS", "MINUS"),
        ("left", "TIMES", "DIVIDE"),
        ("right", "UMINUS"),
    )

    # REGRAS DA GRAMÁTICA
    # =========================================================================

    def p_program(self, p):
        """program : program_units"""
        units = p[1]
        main = next((u for u in units if isinstance(u, Program)), None)
        subprograms = [u for u in units if not isinstance(u, Program)]
        if main is not None:
            main.statements = main.statements + subprograms
            p[0] = main
        else:
            p[0] = units[0] if len(units) == 1 else units

    def p_program_units(self, p):
        """program_units : program_unit
        | program_units program_unit"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[2]]

    def p_program_unit(self, p):
        """program_unit : main_program
        | subroutine
        | function"""
        p[0] = p[1]

    def p_main_program(self, p):
        """main_program : PROGRAM IDENTIFIER statements END
        | PROGRAM IDENTIFIER END"""
        if len(p) == 5:
            p[0] = Program(p[2], p[3], lineno=p.lineno(1))
        else:
            p[0] = Program(p[2], [], lineno=p.lineno(1))

    def p_statements_list(self, p):
        """statements : statement
        | statements statement"""
        if len(p) == 2:
            stmts = [p[1]] if p[1] is not None else []
            p[0] = stmts
        else:
            if p[2] is not None:
                p[0] = p[1] + [p[2]]
            else:
                p[0] = p[1]

    def p_statement(self, p):
        """statement : declaration
        | assignment
        | print_stmt
        | read_stmt
        | if_stmt
        | do_loop
        | goto_stmt
        | call_stmt
        | return_stmt"""
        p[0] = p[1]

    # DECLARAÇÕES
    def p_declaration(self, p):
        """declaration : type_spec var_list"""
        p[0] = Declaration(p[1], p[2], lineno=p.lineno(1))

    def p_type_spec(self, p):
        """type_spec : INTEGER
        | REAL
        | LOGICAL"""
        p[0] = p[1]

    def p_var_list_single(self, p):
        """var_list : IDENTIFIER"""
        p[0] = [p[1]]

    def p_var_list_multi(self, p):
        """var_list : var_list COMMA IDENTIFIER"""
        p[0] = p[1] + [p[3]]

    # ATRIBUIÇÃO
    def p_assignment(self, p):
        """assignment : IDENTIFIER ASSIGN expression"""
        p[0] = Assignment(p[1], p[3], lineno=p.lineno(1))

    # PRINT
    def p_print_stmt(self, p):
        """print_stmt : PRINT TIMES print_list"""
        p[0] = PrintStatement(p[3], lineno=p.lineno(1))

    def p_print_list(self, p):
        """print_list : COMMA expression
        | print_list COMMA expression"""
        if len(p) == 3:
            p[0] = [p[2]]
        else:
            p[0] = p[1] + [p[3]]

    # READ
    def p_read_stmt(self, p):
        """read_stmt : READ TIMES read_list"""
        p[0] = ReadStatement(p[3], lineno=p.lineno(1))

    def p_read_list(self, p):
        """read_list : COMMA IDENTIFIER
        | read_list COMMA IDENTIFIER"""
        if len(p) == 3:
            p[0] = [p[2]]
        else:
            p[0] = p[1] + [p[3]]

    # IF STATEMENT
    def p_if_then_endif(self, p):
        """if_stmt : IF LPAREN expression RPAREN THEN statements ENDIF"""
        p[0] = IfStatement(p[3], p[6], None, lineno=p.lineno(1))

    def p_if_then_else_endif(self, p):
        """if_stmt : IF LPAREN expression RPAREN THEN statements ELSE statements ENDIF"""
        p[0] = IfStatement(p[3], p[6], p[8], lineno=p.lineno(1))

    # DO LOOP
    def p_do_loop(self, p):
        """do_loop : DO NUMBER IDENTIFIER ASSIGN expression COMMA expression statements NUMBER CONTINUE"""
        statements = p[8]
        statements.append(ContinueStatement(int(p[9]), lineno=p.lineno(9)))
        p[0] = DoLoop(int(p[2]), p[3], p[5], p[7], statements, lineno=p.lineno(1))

    # GOTO
    def p_goto_stmt(self, p):
        """goto_stmt : GOTO NUMBER"""
        p[0] = GotoStatement(int(p[2]), lineno=p.lineno(1))

    # EXPRESSÕES
    def p_expression(self, p):
        """expression : expr_or"""
        p[0] = p[1]

    def p_expr_or(self, p):
        """expr_or : expr_and
        | expr_or OR expr_and"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = BinaryOp(p[1], ".OR.", p[3], lineno=p.lineno(1))

    def p_expr_and(self, p):
        """expr_and : expr_not
        | expr_and AND expr_not"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = BinaryOp(p[1], ".AND.", p[3], lineno=p.lineno(1))

    def p_expr_not(self, p):
        """expr_not : expr_rel
        | NOT expr_not"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = UnaryOp(".NOT.", p[2], lineno=p.lineno(1))

    def p_expr_rel(self, p):
        """expr_rel : expr_arith
        | expr_arith EQ expr_arith
        | expr_arith NE expr_arith
        | expr_arith LT expr_arith
        | expr_arith LE expr_arith
        | expr_arith GT expr_arith
        | expr_arith GE expr_arith"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = BinaryOp(p[1], p[2], p[3], lineno=p.lineno(1))

    def p_expr_arith(self, p):
        """expr_arith : term
        | expr_arith PLUS term
        | expr_arith MINUS term"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = BinaryOp(p[1], p[2], p[3], lineno=p.lineno(1))

    def p_term(self, p):
        """term : factor
        | term TIMES factor
        | term DIVIDE factor"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = BinaryOp(p[1], p[2], p[3], lineno=p.lineno(1))

    def p_factor(self, p):
        """factor : primary
        | MINUS factor %prec UMINUS"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = UnaryOp("-", p[2], lineno=p.lineno(1))

    def p_primary(self, p):
        """primary : NUMBER
        | REAL_NUMBER
        | STRING
        | TRUE
        | FALSE
        | IDENTIFIER
        | LPAREN expression RPAREN"""
        if len(p) == 2:
            if isinstance(p[1], bool):
                p[0] = BooleanLiteral(p[1], lineno=p.lineno(1))

            elif isinstance(p[1], int):
                p[0] = IntLiteral(p[1], lineno=p.lineno(1))

            elif isinstance(p[1], float):
                p[0] = RealLiteral(p[1], lineno=p.lineno(1))

            elif isinstance(p[1], str):
                token_type = p.slice[1].type

                if token_type == "STRING":
                    p[0] = StringLiteral(p[1], lineno=p.lineno(1))
                else:  # IDENTIFIER
                    p[0] = Variable(p[1], lineno=p.lineno(1))
        else:
            p[0] = p[2]

    # =========================================================================
    # SUBPROGRAMAS
    # =========================================================================

    def p_subroutine(self, p):
        """subroutine : SUBROUTINE IDENTIFIER LPAREN param_list RPAREN statements END
        | SUBROUTINE IDENTIFIER statements END"""
        if len(p) == 8:
            p[0] = Subroutine(p[2], p[4], p[6], lineno=p.lineno(1))
        else:
            p[0] = Subroutine(p[2], [], p[3], lineno=p.lineno(1))

    def p_function(self, p):
        """function : type_spec FUNCTION IDENTIFIER LPAREN param_list RPAREN statements END
        | type_spec FUNCTION IDENTIFIER statements END"""
        if len(p) == 9:
            p[0] = Function(p[1], p[3], p[5], p[7], lineno=p.lineno(1))
        else:
            p[0] = Function(p[1], p[3], [], p[4], lineno=p.lineno(1))

    def p_param_list(self, p):
        """param_list : IDENTIFIER
        | param_list COMMA IDENTIFIER"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    def p_call_stmt(self, p):
        """call_stmt : CALL IDENTIFIER LPAREN arg_list RPAREN
        | CALL IDENTIFIER"""
        if len(p) == 6:
            p[0] = CallStatement(p[2], p[4], lineno=p.lineno(1))
        else:
            p[0] = CallStatement(p[2], [], lineno=p.lineno(1))

    def p_return_stmt(self, p):
        """return_stmt : RETURN"""
        p[0] = ReturnStatement(lineno=p.lineno(1))

    def p_arg_list(self, p):
        """arg_list : expression
        | arg_list COMMA expression"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[0] = p[1] + [p[3]]

    # Modify primary to support function calls
    def p_primary_func_call(self, p):
        """primary : IDENTIFIER LPAREN arg_list RPAREN"""
        p[0] = FunctionCall(p[1], p[3], lineno=p.lineno(1))

    # =========================================================================
    # ERRO
    # =========================================================================

    def p_error(self, p):
        if p:
            raise ParserError(
                f"Erro sintático em '{p.value}'",
                lineno=p.lineno,
                value=p.value,
            )
        else:
            raise ParserError("Erro sintático no fim do input")

    def parse(self, source_code):
        """
        Analisa código fonte e retorna AST.

        Args:
            source_code (str): Código Fortran

        Returns:
            Program: Raiz da AST
        """
        self.lexer.build()
        self.lexer.lexer.input(source_code)

        try:
            return self.parser.parse(lexer=self.lexer.lexer, debug=False)
        except ParserError:
            raise


def create_parser():
    """Factory function para criar novo parser"""
    return FortranParser()
