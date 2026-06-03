import sys

def parse_tree(tokens):
    if not tokens:
        raise ValueError("Entrada inválida")

    tok = tokens.pop(0)

    # Se for um número → folha
    if tok != 'x':
        return 1

    # Caso seja 'x' → nó interno
    left = parse_tree(tokens)
    right = parse_tree(tokens)

    return 1 + max(left, right)


def main():
    if len(sys.argv) != 2:
        print("Uso: python script.py ficheiro.tree")
        return

    filename = sys.argv[1]

    with open(filename, 'r') as f:
        content = f.read()

    tokens = content.split()

    depth = parse_tree(tokens)

    print(depth)


if __name__ == "__main__":
    main()
