#!/usr/bin/env python3
"""
Compilador Fortran 77 para VM Académica - Entry Point

Uso:
    python main.py <arquivo.f90>              # Compila programa
    python main.py <arquivo.f90> --tokenize   # Mostra tokens
    python main.py <arquivo.f90> --parse      # Mostra AST
    python main.py --help                     # Mostra ajuda
"""

import argparse
import sys
from pathlib import Path

from src.ast_nodes import print_ast
from src.codegen import generate_to_file
from src.lexer import FortranLexer
from src.parser import FortranParser
from src.semantic import SemanticAnalyzer


def tokenize_file(filepath, verbose=False):
    """
    Tokeniza um arquivo Fortran e imprime os tokens.

    Args:
        filepath (str): Caminho para o arquivo .f90
        verbose (bool): Mostrar mais detalhes
    """
    try:
        with open(filepath, "r", encoding="utf-8") as f:
            source_code = f.read()
    except FileNotFoundError:
        print(f"Erro: Arquivo '{filepath}' não encontrado.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Erro ao ler arquivo: {e}", file=sys.stderr)
        sys.exit(1)

    lexer = FortranLexer()
    tokens = lexer.tokenize(source_code)

    print(f"\n{'=' * 70}")
    print(f"{'TOKENIZAÇÃO':<70}")
    print(f"{'=' * 70}")
    print(f"Arquivo: {filepath}")
    print(f"Total de tokens: {len(tokens)}\n")

    print(f"{'Tipo':<20} {'Valor':<30} {'Linha':<5}")
    print("-" * 55)

    for tok in tokens:
        valor_str = str(tok.value)
        if len(valor_str) > 27:
            valor_str = valor_str[:24] + "..."

        if verbose or tok.type != "NEWLINE":
            print(f"{tok.type:<20} {valor_str:<30} {tok.lineno:<5}")

    print(f"{'=' * 70}\n")


def semantic_file(filepath):
    """
    Analisa um arquivo Fortran (análise semântica).

    Args:
        filepath (str): Caminho para o arquivo .f90
    """
    try:
        with open(filepath, "r", encoding="utf-8") as f:
            source_code = f.read()
    except FileNotFoundError:
        print(f"Erro: Arquivo '{filepath}' não encontrado.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Erro ao ler arquivo: {e}", file=sys.stderr)
        sys.exit(1)

    parser = FortranParser()
    ast = parser.parse(source_code)

    print(f"\n{'=' * 70}")
    print(f"{'ANÁLISE SEMÂNTICA':<70}")
    print(f"{'=' * 70}")
    print(f"Arquivo: {filepath}\n")

    if ast is None:
        print("Erro: Não foi possível gerar AST")
        sys.exit(1)

    analyzer = SemanticAnalyzer()
    success = analyzer.analyze(ast)

    # Mostrar tabela de símbolos
    analyzer.print_symbols()

    # Mostrar erros e avisos
    analyzer.print_errors()

    if not success:
        print(f"{'=' * 70}\n")
        sys.exit(1)

    print(f"{'=' * 70}\n")


def parse_file(filepath):
    """
    Parseia um arquivo Fortran e imprime a AST.

    Args:
        filepath (str): Caminho para o arquivo .f90
    """
    try:
        with open(filepath, "r", encoding="utf-8") as f:
            source_code = f.read()
    except FileNotFoundError:
        print(f"Erro: Arquivo '{filepath}' não encontrado.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Erro ao ler arquivo: {e}", file=sys.stderr)
        sys.exit(1)

    parser = FortranParser()
    ast = parser.parse(source_code)

    print(f"\n{'=' * 70}")
    print(f"{'ANÁLISE SINTÁTICA':<70}")
    print(f"{'=' * 70}")
    print(f"Arquivo: {filepath}\n")

    if ast is None:
        print("Erro: Não foi possível gerar AST")
        sys.exit(1)

    print("AST (Árvore de Sintaxe Abstrata):\n")
    print_ast(ast)
    print(f"\n{'=' * 70}\n")


def compile_file(filepath):
    """
    Compila um arquivo Fortran para código VM.

    Args:
        filepath (str): Caminho para o arquivo .f90
    """
    try:
        with open(filepath, "r", encoding="utf-8") as f:
            source_code = f.read()
    except FileNotFoundError:
        print(f"Erro: Arquivo '{filepath}' não encontrado.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Erro ao ler arquivo: {e}", file=sys.stderr)
        sys.exit(1)

    print(f"\n{'=' * 70}")
    print(f"{'COMPILAÇÃO FORTRAN 77':<70}")
    print(f"{'=' * 70}")
    print(f"Arquivo: {filepath}\n")

    # Fase 1: Lexer
    lexer = FortranLexer()
    tokens = lexer.tokenize(source_code)
    print(f"Fase 1 - Análise Léxica: {len(tokens)} tokens")

    # Fase 2: Parser
    parser = FortranParser()
    ast = parser.parse(source_code)
    if ast:
        print("Fase 2 - Análise Sintática: OK")
    else:
        print("Fase 2 - Análise Sintática: ERRO")
        sys.exit(1)

    # Fase 3: Análise Semântica
    analyzer = SemanticAnalyzer()
    success = analyzer.analyze(ast)

    if success:
        print("Fase 3 - Análise Semântica: OK")
    else:
        print("Fase 3 - Análise Semântica: ERROS ENCONTRADOS")
        analyzer.print_errors()
        sys.exit(1)

    if analyzer.has_warnings():
        print(f"Fase 3 - Análise Semântica: {len(analyzer.warnings)} aviso(s)")
        analyzer.print_errors()

    # Mostrar tabela de símbolos
    analyzer.print_symbols()

    # Fase 4: Geração de Código
    import os

    filepath_str = str(filepath).replace("\\", "/")
    if "tests/programs" in filepath_str:
        out_dir = os.path.join(
            os.path.dirname(os.path.dirname(str(filepath))), "results"
        )
        os.makedirs(out_dir, exist_ok=True)
        output_file = os.path.join(
            out_dir, os.path.basename(str(filepath)).replace(".f90", ".vm")
        )
    else:
        output_file = str(filepath).replace(".f90", ".vm")
    success = generate_to_file(ast, output_file, analyzer)

    if success:
        print(f"Fase 4 - Geração de Código: {output_file}")
    else:
        print("Fase 4 - Geração de Código: ERRO")
        sys.exit(1)

    print(f"\n{'=' * 70}")
    print("Compilação concluída com sucesso!")
    print(f"Ficheiro de saída: {output_file}")
    print(f"{'=' * 70}\n")


def main():
    """Entry point principal"""
    parser = argparse.ArgumentParser(
        description="Compilador Fortran 77 para VM Académica",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Exemplos:
  %(prog)s programa.f90              # Compila programa
  %(prog)s programa.f90 --tokenize   # Mostra apenas tokens
  %(prog)s programa.f90 --parse      # Mostra AST
  %(prog)s --help                    # Mostra ajuda
        """,
    )

    parser.add_argument("file", nargs="?", help="Arquivo fonte Fortran 77 (.f90)")

    parser.add_argument(
        "--tokenize",
        action="store_true",
        help="Mostra apenas a tokenização léxica",
    )

    parser.add_argument(
        "--parse",
        action="store_true",
        help="Mostra a árvore sintática (AST)",
    )

    parser.add_argument(
        "--semantic",
        action="store_true",
        help="Mostra análise semântica (tabela de símbolos, erros)",
    )

    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Modo verbose (mais detalhes)",
    )

    parser.add_argument(
        "--version",
        action="version",
        version="%(prog)s 0.1.0 (Fase 5: Geração de Código VM)",
    )

    args = parser.parse_args()

    if not args.file:
        parser.print_help()
        sys.exit(0)

    filepath = Path(args.file)

    if not filepath.exists():
        print(f"Erro: Arquivo '{filepath}' não encontrado.", file=sys.stderr)
        sys.exit(1)

    if args.tokenize:
        tokenize_file(filepath, args.verbose)
    elif args.parse:
        parse_file(filepath)
    elif args.semantic:
        semantic_file(filepath)
    else:
        compile_file(filepath)


if __name__ == "__main__":
    main()
