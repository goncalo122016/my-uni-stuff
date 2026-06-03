"""
Análise Semântica para Fortran 77

Responsabilidades:
- Tabela de símbolos (variáveis declaradas)
- Verificação de declarações
- Validação de labels (DO, GOTO, CONTINUE)
- Validação de tipos básicos
- Geração de mensagens de erro com contexto (linha, coluna)
"""

from typing import Dict, List, Optional, Set, Tuple

from src.ast_nodes import (
    Subroutine,
    Function,
    CallStatement,
    ReturnStatement,
    FunctionCall,
    Assignment,
    ASTNode,
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


class Symbol:
    """Representa um símbolo (variável) na tabela de símbolos"""

    def __init__(self, name: str, type_spec: str, line: int):
        self.name = name
        self.type_spec = type_spec  # 'INTEGER', 'REAL', 'LOGICAL'
        self.line = line  # Linha onde foi declarada
        self.used = False  # Se foi usada
        self.assigned = False  # Se foi atribuída

    def __repr__(self):
        return f"Symbol({self.name}, {self.type_spec}, line={self.line})"


class SemanticError:
    """Representa um erro semântico"""

    def __init__(self, line: int, message: str, error_type: str = "ERRO"):
        self.line = line
        self.message = message
        self.error_type = error_type  # "ERRO" ou "AVISO"

    def __repr__(self):
        return f"[{self.error_type}] Linha {self.line}: {self.message}"


class SemanticAnalyzer:
    """Analisador semântico para Fortran 77"""

    def __init__(self):
        self.symbols: Dict[str, Symbol] = {}
        self.errors: List[SemanticError] = []
        self.warnings: List[SemanticError] = []
        self.labels: Set[int] = set()  # Labels encontrados (CONTINUE)
        self.goto_labels: Set[int] = set()  # Labels usados por GOTO/DO
        self.do_labels: Set[int] = set()  # Labels de DO loops

    def analyze(self, ast: Program) -> bool:
        """
        Analisa a AST e popula tabela de símbolos.

        Args:
            ast: Árvore sintática (Program)

        Returns:
            bool: True se sem erros, False caso contrário
        """
        if ast is None:
            self.errors.append(SemanticError(0, "AST é None"))
            return False

        self.errors = []
        self.warnings = []
        self.symbols = {}
        self.labels = set()
        self.goto_labels = set()
        self.do_labels = set()

        # Primeira passagem: coletar declarações
        self._collect_declarations(ast)

        # Segunda passagem: coletar labels
        self._collect_labels(ast)

        # Terceira passagem: validar uso de variáveis e labels
        self._validate_statements(ast)

        # Quarta passagem: avisos sobre variáveis não usadas
        self._check_unused_variables()

        # Validar labels de GOTO
        self._validate_goto_labels()

        return len(self.errors) == 0

    def _collect_declarations(self, node: ASTNode) -> None:
        """Coleta todas as declarações de variáveis"""
        if isinstance(node, list):
            for stmt in node:
                self._collect_declarations(stmt)
        elif isinstance(node, Program):
            for stmt in node.statements:
                self._collect_declarations(stmt)
        elif isinstance(node, (Subroutine, Function)):
            if isinstance(node, Function):
                self.symbols[node.name] = Symbol(node.name, node.type_spec, node.lineno)
                self.symbols[node.name].assigned = True # implicit
            else:
                self.symbols[node.name] = Symbol(node.name, "SUBROUTINE", node.lineno)
                self.symbols[node.name].assigned = True
                
            for param in node.params:
                if param not in self.symbols:
                    self.symbols[param] = Symbol(param, "PARAM", node.lineno)
                    self.symbols[param].assigned = True
            for stmt in node.body:
                self._collect_declarations(stmt)

        elif isinstance(node, Declaration):
            for var_name in node.variables:
                # Verificar redeclaração
                if var_name in self.symbols:
                    if self.symbols[var_name].type_spec == "PARAM":
                        self.symbols[var_name].type_spec = node.type_spec
                    else:
                        self.errors.append(
                            SemanticError(
                                node.lineno,
                                f"Variável '{var_name}' declarada múltiplas vezes",
                            )
                        )
                else:
                    self.symbols[var_name] = Symbol(
                        var_name, node.type_spec, node.lineno
                    )

        elif isinstance(node, (IfStatement, DoLoop)):
            if isinstance(node, IfStatement):
                for stmt in node.then_branch:
                    self._collect_declarations(stmt)
                if node.else_branch:
                    for stmt in node.else_branch:
                        self._collect_declarations(stmt)
            elif isinstance(node, DoLoop):
                for stmt in node.body:
                    self._collect_declarations(stmt)

    def _collect_labels(self, node: ASTNode) -> None:
        """Coleta todos os labels (CONTINUE, DO, GOTO)"""
        if isinstance(node, list):
            for stmt in node:
                self._collect_labels(stmt)
        elif isinstance(node, Program):
            for stmt in node.statements:
                self._collect_labels(stmt)
        elif isinstance(node, (Subroutine, Function)):
            for stmt in node.body:
                self._collect_labels(stmt)

        elif isinstance(node, ContinueStatement):
            if node.label in self.labels:
                self.errors.append(
                    SemanticError(node.lineno, f"Label {node.label} duplicado")
                )
            self.labels.add(node.label)

        elif isinstance(node, DoLoop):
            self.do_labels.add(node.label)
            for stmt in node.body:
                self._collect_labels(stmt)

        elif isinstance(node, GotoStatement):
            self.goto_labels.add(node.label)

        elif isinstance(node, (IfStatement,)):
            if node.then_branch:
                for stmt in node.then_branch:
                    self._collect_labels(stmt)
            if node.else_branch:
                for stmt in node.else_branch:
                    self._collect_labels(stmt)

    def _validate_statements(self, node: ASTNode) -> None:
        """Valida uso de variáveis e tipos"""
        if isinstance(node, list):
            for stmt in node:
                self._validate_statements(stmt)
        elif isinstance(node, Program):
            for stmt in node.statements:
                self._validate_statements(stmt)
        elif isinstance(node, (Subroutine, Function)):
            for stmt in node.body:
                self._validate_statements(stmt)

        elif isinstance(node, Assignment):
            self._validate_expression(node.value)
            if node.target not in self.symbols:
                self.errors.append(
                    SemanticError(
                        node.lineno, f"Variável '{node.target}' não declarada"
                    )
                )
            else:
                self.symbols[node.target].assigned = True
                target_type = self.symbols[node.target].type_spec
                value_type = self._get_expr_type(node.value)
                if value_type is not None:
                    if target_type in ("INTEGER", "REAL") and value_type == "LOGICAL":
                        self.errors.append(SemanticError(
                            node.lineno,
                            f"Não é possível atribuir valor LOGICAL a variável {target_type} '{node.target}'"
                        ))
                    elif target_type == "LOGICAL" and value_type in ("INTEGER", "REAL"):
                        self.errors.append(SemanticError(
                            node.lineno,
                            f"Não é possível atribuir valor {value_type} a variável LOGICAL '{node.target}'"
                        ))
                    elif target_type == "INTEGER" and value_type == "REAL":
                        self.warnings.append(SemanticError(
                            node.lineno,
                            f"Atribuição de REAL a INTEGER '{node.target}': possível truncamento",
                            error_type="AVISO"
                        ))

        elif isinstance(node, PrintStatement):
            for expr in node.expressions:
                self._validate_expression(expr)

        elif isinstance(node, ReadStatement):
            for var_name in node.variables:
                if var_name not in self.symbols:
                    self.errors.append(
                        SemanticError(
                            node.lineno, f"Variável '{var_name}' não declarada"
                        )
                    )
                else:
                    self.symbols[var_name].used = True
                    self.symbols[var_name].assigned = True

        elif isinstance(node, IfStatement):
            self._validate_expression(node.condition)
            for stmt in node.then_branch:
                self._validate_statements(stmt)
            if node.else_branch:
                for stmt in node.else_branch:
                    self._validate_statements(stmt)

        elif isinstance(node, DoLoop):
            # Validar que loop_var está declarada
            if node.variable not in self.symbols:
                self.errors.append(
                    SemanticError(
                        node.lineno, f"Variável '{node.variable}' não declarada"
                    )
                )
            else:
                self.symbols[node.variable].used = True
                self.symbols[node.variable].assigned = True

            # Validar expressões de start/end
            self._validate_expression(node.start)
            self._validate_expression(node.end)

            # Validar statements no body
            for stmt in node.body:
                self._validate_statements(stmt)

        elif isinstance(node, CallStatement):
            if node.name not in self.symbols:
                self.errors.append(SemanticError(node.lineno, f"Subrotina '{node.name}' não declarada"))
            else:
                self.symbols[node.name].used = True
            for arg in node.args:
                self._validate_expression(arg)
        elif isinstance(node, ReturnStatement):
            pass
        elif isinstance(node, (ContinueStatement, GotoStatement)):
            pass

    def _get_expr_type(self, expr: ASTNode) -> Optional[str]:
        """Infere o tipo de uma expressão sem gerar erros."""
        if isinstance(expr, IntLiteral):
            return "INTEGER"
        elif isinstance(expr, RealLiteral):
            return "REAL"
        elif isinstance(expr, StringLiteral):
            return "CHARACTER"
        elif isinstance(expr, BooleanLiteral):
            return "LOGICAL"
        elif isinstance(expr, Variable):
            sym = self.symbols.get(expr.name)
            t = sym.type_spec if sym else None
            return t if t in ("INTEGER", "REAL", "LOGICAL", "CHARACTER") else None
        elif isinstance(expr, UnaryOp):
            if expr.op == ".NOT.":
                return "LOGICAL"
            elif expr.op == "-":
                t = self._get_expr_type(expr.operand)
                return t if t in ("INTEGER", "REAL") else None
        elif isinstance(expr, BinaryOp):
            if expr.op in ("+", "-", "*", "/"):
                lt = self._get_expr_type(expr.left)
                rt = self._get_expr_type(expr.right)
                if lt == "REAL" or rt == "REAL":
                    return "REAL"
                if lt == "INTEGER" or rt == "INTEGER":
                    return "INTEGER"
                return None
            elif expr.op in (".EQ.", ".NE.", ".LT.", ".LE.", ".GT.", ".GE.",
                             ".AND.", ".OR."):
                return "LOGICAL"
        elif isinstance(expr, FunctionCall):
            sym = self.symbols.get(expr.name)
            t = sym.type_spec if sym else None
            return t if t in ("INTEGER", "REAL", "LOGICAL", "CHARACTER") else None
        return None

    def _validate_expression(self, expr: ASTNode) -> None:
        """Valida uma expressão: verifica declarações e tipos."""
        if isinstance(expr, Variable):
            if expr.name not in self.symbols:
                self.errors.append(
                    SemanticError(expr.lineno, f"Variável '{expr.name}' não declarada")
                )
            else:
                self.symbols[expr.name].used = True

        elif isinstance(expr, BinaryOp):
            self._validate_expression(expr.left)
            self._validate_expression(expr.right)
            self._check_binary_op_types(expr)

        elif isinstance(expr, UnaryOp):
            self._validate_expression(expr.operand)
            self._check_unary_op_type(expr)

        elif isinstance(expr, FunctionCall):
            if expr.name not in self.symbols:
                self.errors.append(SemanticError(expr.lineno, f"Função '{expr.name}' não declarada"))
            else:
                self.symbols[expr.name].used = True
            for arg in expr.args:
                self._validate_expression(arg)

    def _check_binary_op_types(self, expr: BinaryOp) -> None:
        """Verifica compatibilidade de tipos num BinaryOp."""
        lt = self._get_expr_type(expr.left)
        rt = self._get_expr_type(expr.right)

        if expr.op in ("+", "-", "*", "/"):
            if lt == "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Operando de tipo LOGICAL não pode ser usado em operação aritmética '{expr.op}'"
                ))
            if rt == "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Operando de tipo LOGICAL não pode ser usado em operação aritmética '{expr.op}'"
                ))
            if lt in ("INTEGER", "REAL") and rt in ("INTEGER", "REAL") and lt != rt:
                self.warnings.append(SemanticError(
                    expr.lineno,
                    f"Mistura de tipos INTEGER e REAL em operação aritmética '{expr.op}'",
                    error_type="AVISO"
                ))

        elif expr.op in (".AND.", ".OR."):
            if lt is not None and lt != "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Operando de tipo {lt} em operação lógica '{expr.op}' (esperado LOGICAL)"
                ))
            if rt is not None and rt != "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Operando de tipo {rt} em operação lógica '{expr.op}' (esperado LOGICAL)"
                ))

        elif expr.op in (".LT.", ".LE.", ".GT.", ".GE."):
            if lt == "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Tipo LOGICAL não pode ser usado em comparação de ordem '{expr.op}'"
                ))
            if rt == "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Tipo LOGICAL não pode ser usado em comparação de ordem '{expr.op}'"
                ))

    def _check_unary_op_type(self, expr: UnaryOp) -> None:
        """Verifica tipo do operando num UnaryOp."""
        t = self._get_expr_type(expr.operand)
        if expr.op == "-":
            if t == "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    "Operando de tipo LOGICAL não pode ser negado aritmeticamente"
                ))
        elif expr.op == ".NOT.":
            if t is not None and t != "LOGICAL":
                self.errors.append(SemanticError(
                    expr.lineno,
                    f"Operando de tipo {t} não pode ser usado com .NOT. (esperado LOGICAL)"
                ))

    def _check_unused_variables(self) -> None:
        """Gera avisos para variáveis declaradas mas nunca usadas"""
        for name, symbol in self.symbols.items():
            if not symbol.used:
                self.warnings.append(
                    SemanticError(
                        symbol.line,
                        f"Variável '{name}' declarada mas nunca usada",
                        error_type="AVISO",
                    )
                )

    def _validate_goto_labels(self) -> None:
        """Valida que todos os GOTO/DO apontam para labels existentes"""
        # Validar GOTO
        for label in self.goto_labels:
            if label not in self.labels and label not in self.do_labels:
                self.errors.append(
                    SemanticError(0, f"Label {label} não encontrado (GOTO)")
                )

        # Validar DO loops têm CONTINUE correspondente
        for label in self.do_labels:
            if label not in self.labels:
                self.errors.append(
                    SemanticError(0, f"Label {label} (DO) sem CONTINUE correspondente")
                )

    def get_symbol(self, name: str) -> Optional[Symbol]:
        """Retorna símbolo ou None"""
        return self.symbols.get(name)

    def is_declared(self, name: str) -> bool:
        """Verifica se variável está declarada"""
        return name in self.symbols

    def get_type(self, name: str) -> Optional[str]:
        """Retorna tipo de variável ou None"""
        if name in self.symbols:
            return self.symbols[name].type_spec
        return None

    def print_symbols(self) -> None:
        """Imprime tabela de símbolos (debug)"""
        print("\n" + "=" * 60)
        print("TABELA DE SÍMBOLOS")
        print("=" * 60)
        print(f"{'Nome':<15} {'Tipo':<12} {'Linha':<8} {'Usado':<8} {'Atrib.':<8}")
        print("-" * 60)

        for name, symbol in self.symbols.items():
            usado = "Sim" if symbol.used else "Não"
            atrib = "Sim" if symbol.assigned else "Não"
            print(
                f"{symbol.name:<15} {symbol.type_spec:<12} "
                f"{symbol.line:<8} {usado:<8} {atrib:<8}"
            )

        print("=" * 60 + "\n")

    def print_errors(self) -> None:
        """Imprime erros e avisos"""
        if self.errors:
            print("\n" + "=" * 60)
            print("ERROS SEMÂNTICOS")
            print("=" * 60)
            for err in self.errors:
                print(f"  {err}")
            print("=" * 60 + "\n")

        if self.warnings:
            print("\n" + "=" * 60)
            print("AVISOS")
            print("=" * 60)
            for warn in self.warnings:
                print(f"  {warn}")
            print("=" * 60 + "\n")

    def has_errors(self) -> bool:
        """Retorna True se houver erros (não avisos)"""
        return len(self.errors) > 0

    def has_warnings(self) -> bool:
        """Retorna True se houver avisos"""
        return len(self.warnings) > 0


def analyze(ast: Program) -> Tuple[bool, SemanticAnalyzer]:
    """
    Interface pública para análise semântica.

    Args:
        ast: Árvore sintática

    Returns:
        Tupla (sucesso, analisador)
    """
    analyzer = SemanticAnalyzer()
    success = analyzer.analyze(ast)
    return success, analyzer
