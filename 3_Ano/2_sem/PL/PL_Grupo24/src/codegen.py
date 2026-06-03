"""
Gerador de código para VM stack-based

Converte AST em instruções VM.

Instruções suportadas:
- PUSHI n, PUSHF x, PUSHS "str"
- PUSHG addr, STOREG addr (variáveis globais)
- PUSHL idx, STOREL idx  (variáveis locais em frame de ativação)
- ADD, SUB, MUL, DIV, NEG
- EQ, NE, LT, LE, GT, GE
- AND, OR, NOT
- LABEL, JUMP, JZ
- WRITEI, WRITEF, WRITES, WRITELN
- READ, ATOI, ATOF
- PUSHA, CALL, RETURN
- HALT
"""

from typing import Dict, List, Optional

from src.ast_nodes import (
    Subroutine,
    Function,
    CallStatement,
    ReturnStatement,
    FunctionCall,
    Assignment,
    BinaryOp,
    BooleanLiteral,
    ContinueStatement,
    Declaration,
    DoLoop,
    GotoStatement,
    IfStatement,
    IntLiteral,
    PrintStatement,
    Program,
    ReadStatement,
    RealLiteral,
    StringLiteral,
    UnaryOp,
    Variable,
)


class CodeGenerator:
    """Gerador de código para VM"""

    def __init__(self):
        self.instructions: List[str] = []
        self.label_counter = 0
        self.global_vars: Dict[str, int] = {}   # var_name -> global address
        self.var_types: Dict[str, str] = {}      # var_name -> type_spec
        self.next_global = 0
        self.symbol_table = None
        self.subprograms: Dict = {}

        # Frame de ativação (activo dentro de subroutine/function)
        self.local_frame: Dict[str, int] = {}
        self.in_subprogram = False
        self.return_local: Optional[int] = None  # índice local da var de retorno

    def generate(self, ast, symbol_table=None) -> str:
        """
        Gera código VM a partir de AST.

        Args:
            ast: Árvore sintática (Program)
            symbol_table: Tabela de símbolos (opcional)

        Returns:
            str: Código VM como string
        """
        self.instructions = []
        self.label_counter = 0
        self.global_vars = {}
        self.var_types = {}
        self.next_global = 0
        self.symbol_table = symbol_table
        self.subprograms = {}
        self.local_frame = {}
        self.in_subprogram = False
        self.return_local = None

        if ast is None:
            return ""

        self._collect_globals(ast)

        num_globals = len(self.global_vars)
        if num_globals > 0:
            self._emit(f"PUSHN {num_globals}")
        self._emit("START")

        self._generate_program(ast)

        return "\n".join(self.instructions)

    def _collect_globals(self, node) -> None:
        """Coleta variáveis e tipos globais (pára na fronteira de subprogramas)."""
        if isinstance(node, list):
            for stmt in node:
                self._collect_globals(stmt)
        elif isinstance(node, Program):
            for stmt in node.statements:
                self._collect_globals(stmt)
        elif isinstance(node, (Subroutine, Function)):
            self.subprograms[node.name] = node
            # Não recursivo: variáveis internas são locais ao subprograma
        elif isinstance(node, Declaration):
            for var in node.variables:
                self.var_types[var] = node.type_spec
                if var not in self.global_vars:
                    self.global_vars[var] = self.next_global
                    self.next_global += 1
        elif isinstance(node, IfStatement):
            for stmt in node.then_branch:
                self._collect_globals(stmt)
            if node.else_branch:
                for stmt in node.else_branch:
                    self._collect_globals(stmt)
        elif isinstance(node, DoLoop):
            for stmt in node.body:
                self._collect_globals(stmt)

    def _build_local_frame(self, node) -> Dict[str, int]:
        """Constrói mapa de índices locais para um subprograma."""
        frame: Dict[str, int] = {}
        idx = 0

        # Parâmetros primeiro (já na stack quando a função é chamada)
        for param in node.params:
            frame[param] = idx
            idx += 1

        # Variáveis locais declaradas no corpo
        for stmt in node.body:
            if isinstance(stmt, Declaration):
                for var in stmt.variables:
                    self.var_types[var] = stmt.type_spec
                    if var not in frame:
                        frame[var] = idx
                        idx += 1

        # Para Function: slot de retorno (o nome da função é a variável de retorno)
        if isinstance(node, Function) and node.name not in frame:
            frame[node.name] = idx

        return frame

    def _emit(self, instruction: str) -> None:
        self.instructions.append(instruction)

    def _get_label(self, prefix: str = "L") -> str:
        self.label_counter += 1
        return f"{prefix}{self.label_counter}"

    def _get_var_type(self, name: str) -> Optional[str]:
        """Retorna o tipo de uma variável (tabela de símbolos tem prioridade)."""
        if self.symbol_table is not None:
            t = self.symbol_table.get_type(name)
            if t is not None:
                return t
        return self.var_types.get(name)

    def _var_load(self, name: str) -> None:
        """Emite instrução de leitura de variável (local ou global)."""
        if self.in_subprogram and name in self.local_frame:
            self._emit(f"PUSHL {self.local_frame[name]}")
        else:
            addr = self.global_vars.get(name, 0)
            self._emit(f"PUSHG {addr}")

    def _var_store(self, name: str) -> None:
        """Emite instrução de escrita de variável (local ou global)."""
        if self.in_subprogram and name in self.local_frame:
            self._emit(f"STOREL {self.local_frame[name]}")
        else:
            addr = self.global_vars.get(name, 0)
            self._emit(f"STOREG {addr}")

    def _generate_program(self, node) -> None:
        """Gera código para programa"""
        if isinstance(node, list):
            nodes = node
        else:
            nodes = [node]

        for n in nodes:
            if isinstance(n, Program):
                # Corpo principal (exclui declarações e subprogramas)
                for stmt in n.statements:
                    if not isinstance(stmt, (Declaration, Subroutine, Function)):
                        self._generate_statement(stmt)
                self._emit("STOP")
                # Subprogramas depois do STOP principal
                for stmt in n.statements:
                    if isinstance(stmt, (Subroutine, Function)):
                        self._generate_subprogram(stmt)
            elif isinstance(n, (Subroutine, Function)):
                self._generate_subprogram(n)

    def _generate_subprogram(self, node) -> None:
        """Gera código para subroutine ou function com frame de ativação."""
        frame = self._build_local_frame(node)
        num_params = len(node.params)
        num_extra = len(frame) - num_params  # slots além dos parâmetros

        self._emit(f"{node.name}:")

        # Alocar slots locais adicionais (parâmetros já estão na stack)
        if num_extra > 0:
            self._emit(f"PUSHN {num_extra}")

        # Guardar contexto anterior e activar frame local
        saved_frame = self.local_frame
        saved_in_sub = self.in_subprogram
        saved_return = self.return_local

        self.local_frame = frame
        self.in_subprogram = True
        self.return_local = frame.get(node.name) if isinstance(node, Function) else None

        for stmt in node.body:
            if not isinstance(stmt, Declaration):
                self._generate_statement(stmt)

        # Function: empurrar valor de retorno antes do RETURN
        if isinstance(node, Function) and self.return_local is not None:
            self._emit(f"PUSHL {self.return_local}")

        self._emit("RETURN")

        # Restaurar contexto
        self.local_frame = saved_frame
        self.in_subprogram = saved_in_sub
        self.return_local = saved_return

    def _generate_statement(self, node) -> None:
        """Gera código para statement"""
        if isinstance(node, Assignment):
            self._generate_assignment(node)

        elif isinstance(node, PrintStatement):
            self._generate_print(node)

        elif isinstance(node, ReadStatement):
            self._generate_read(node)

        elif isinstance(node, IfStatement):
            self._generate_if(node)

        elif isinstance(node, DoLoop):
            self._generate_do_loop(node)

        elif isinstance(node, ContinueStatement):
            self._emit(f"{node.label}:")

        elif isinstance(node, GotoStatement):
            self._emit(f"JUMP {node.label}")

        elif isinstance(node, CallStatement):
            self._generate_call(node)

        elif isinstance(node, ReturnStatement):
            # RETURN explícito dentro de function: empurra valor de retorno
            if self.return_local is not None:
                self._emit(f"PUSHL {self.return_local}")
            self._emit("RETURN")

    def _generate_call(self, node: CallStatement) -> None:
        """Gera CALL a subroutine (sem valor de retorno)."""
        subprg = self.subprograms.get(node.name)
        if subprg:
            for arg in node.args:
                self._generate_expression(arg)
        self._emit(f"PUSHA {node.name}")
        self._emit("CALL")

    def _generate_assignment(self, node: Assignment) -> None:
        """Gera código para atribuição: var = expr"""
        self._generate_expression(node.value)
        self._var_store(node.target)

    def _generate_expression(self, node) -> None:
        """Gera código para expressão"""
        if isinstance(node, IntLiteral):
            self._emit(f"PUSHI {node.value}")

        elif isinstance(node, RealLiteral):
            self._emit(f"PUSHF {node.value}")

        elif isinstance(node, StringLiteral):
            escaped = node.value.replace('"', '\\"')
            self._emit(f'PUSHS "{escaped}"')

        elif isinstance(node, BooleanLiteral):
            self._emit(f"PUSHI {1 if node.value else 0}")

        elif isinstance(node, Variable):
            self._var_load(node.name)

        elif isinstance(node, UnaryOp):
            self._generate_unary_op(node)

        elif isinstance(node, BinaryOp):
            self._generate_binary_op(node)

        elif isinstance(node, FunctionCall):
            self._generate_function_call(node)

    def _generate_function_call(self, node: FunctionCall) -> None:
        """Gera chamada a function (deixa valor de retorno na stack)."""
        subprg = self.subprograms.get(node.name)
        if subprg:
            for arg in node.args:
                self._generate_expression(arg)
        self._emit(f"PUSHA {node.name}")
        self._emit("CALL")
        # O valor de retorno foi empurrado pela função antes do RETURN

    def _generate_unary_op(self, node: UnaryOp) -> None:
        """Gera código para operação unária"""
        self._generate_expression(node.operand)

        if node.op == "-":
            self._emit("PUSHI 0")
            self._emit("SWAP")
            self._emit("SUB")
        elif node.op == ".NOT.":
            self._emit("NOT")

    def _generate_binary_op(self, node: BinaryOp) -> None:
        """Gera código para operação binária"""
        self._generate_expression(node.left)
        self._generate_expression(node.right)

        if node.op == "+":
            self._emit("ADD")
        elif node.op == "-":
            self._emit("SUB")
        elif node.op == "*":
            self._emit("MUL")
        elif node.op == "/":
            self._emit("DIV")
        elif node.op == ".EQ.":
            self._emit("EQUAL")
        elif node.op == ".NE.":
            self._emit("EQUAL")
            self._emit("NOT")
        elif node.op == ".LT.":
            self._emit("INF")
        elif node.op == ".LE.":
            self._emit("INFEQ")
        elif node.op == ".GT.":
            self._emit("SUP")
        elif node.op == ".GE.":
            self._emit("SUPEQ")
        elif node.op == ".AND.":
            self._emit("AND")
        elif node.op == ".OR.":
            self._emit("OR")

    def _generate_print(self, node: PrintStatement) -> None:
        """Gera código para PRINT"""
        for expr in node.expressions:
            self._generate_expression(expr)
            if isinstance(expr, StringLiteral):
                self._emit("WRITES")
            elif isinstance(expr, RealLiteral):
                self._emit("WRITEF")
            elif isinstance(expr, Variable):
                # Consulta o tipo real da variável para escolher WRITEF vs WRITEI
                var_type = self._get_var_type(expr.name)
                if var_type == "REAL":
                    self._emit("WRITEF")
                else:
                    self._emit("WRITEI")
            else:
                self._emit("WRITEI")
        self._emit("WRITELN")

    def _generate_read(self, node: ReadStatement) -> None:
        """Gera código para READ"""
        for var in node.variables:
            self._emit("READ")
            # Escolhe ATOF ou ATOI consoante o tipo da variável
            var_type = self._get_var_type(var)
            if var_type == "REAL":
                self._emit("ATOF")
            else:
                self._emit("ATOI")
            self._var_store(var)

    def _generate_if(self, node: IfStatement) -> None:
        """Gera código para IF/ELSE/ENDIF"""
        self._generate_expression(node.condition)

        else_label = self._get_label("ELSE")
        endif_label = self._get_label("ENDIF")

        self._emit(f"JZ {else_label}")

        for stmt in node.then_branch:
            self._generate_statement(stmt)

        self._emit(f"JUMP {endif_label}")

        self._emit(f"{else_label}:")
        if node.else_branch:
            for stmt in node.else_branch:
                self._generate_statement(stmt)

        self._emit(f"{endif_label}:")

    def _generate_do_loop(self, node: DoLoop) -> None:
        """Gera código para DO loop"""
        self._generate_expression(node.start)
        self._var_store(node.variable)

        loop_start = self._get_label("LOOP")
        self._emit(f"{loop_start}:")

        self._var_load(node.variable)
        self._generate_expression(node.end)
        self._emit("INFEQ")

        loop_end = self._get_label("ENDLOOP")
        self._emit(f"JZ {loop_end}")

        for stmt in node.body:
            if isinstance(stmt, ContinueStatement) and stmt.label == node.label:
                self._var_load(node.variable)
                self._emit("PUSHI 1")
                self._emit("ADD")
                self._var_store(node.variable)
                self._emit(f"JUMP {loop_start}")
            else:
                self._generate_statement(stmt)

        self._var_load(node.variable)
        self._emit("PUSHI 1")
        self._emit("ADD")
        self._var_store(node.variable)
        self._emit(f"JUMP {loop_start}")

        self._emit(f"{loop_end}:")


def generate_code(ast: Program, symbol_table=None) -> str:
    """
    Interface pública para geração de código.

    Args:
        ast: Árvore sintática
        symbol_table: Tabela de símbolos (opcional)

    Returns:
        str: Código VM
    """
    gen = CodeGenerator()
    return gen.generate(ast, symbol_table)


def generate_to_file(ast: Program, output_file: str, symbol_table=None) -> bool:
    """
    Gera código e salva em ficheiro.

    Args:
        ast: Árvore sintática
        output_file: Ficheiro de output
        symbol_table: Tabela de símbolos (opcional)

    Returns:
        bool: Sucesso
    """
    try:
        code = generate_code(ast, symbol_table)
        with open(output_file, "w", newline="\n") as f:
            f.write(code)
        return True
    except Exception as e:
        print(f"Erro ao gerar código: {e}")
        return False
