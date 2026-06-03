import sys
import random

def bad_prng(n):
    """Gerador de números pseudo-aleatórios INSEGURO."""
    random.seed(random.randbytes(2))
    return random.randbytes(n)

def main():
    if len(sys.argv) < 2:
        print(f"Uso: {sys.argv[0]} setup|enc|dec ...", file=sys.stderr)
        sys.exit(1)

    mode = sys.argv[1]

    if mode == 'setup':
        # setup <n_bytes> <keyfile>
        n       = int(sys.argv[2])
        keyfile = sys.argv[3]
        key = bad_prng(n)
        with open(keyfile, 'wb') as f:
            f.write(key)

    elif mode == 'enc':
        # enc <msgfile> <keyfile>
        msgfile = sys.argv[2]
        keyfile = sys.argv[3]
        with open(msgfile, 'rb') as f:
            msg = f.read()
        with open(keyfile, 'rb') as f:
            key = f.read()
        if len(key) < len(msg):
            print("Erro: chave mais curta que a mensagem", file=sys.stderr)
            sys.exit(1)
        ctxt = bytes(m ^ k for m, k in zip(msg, key))
        sys.stdout.buffer.write(ctxt)

    elif mode == 'dec':
        # dec <ctxtfile> <keyfile>
        ctxtfile = sys.argv[2]
        keyfile  = sys.argv[3]
        with open(ctxtfile, 'rb') as f:
            ctxt = f.read()
        with open(keyfile, 'rb') as f:
            key = f.read()
        if len(key) < len(ctxt):
            print("Erro: chave mais curta que o criptograma", file=sys.stderr)
            sys.exit(1)
        plain = bytes(c ^ k for c, k in zip(ctxt, key))
        sys.stdout.buffer.write(plain)

    else:
        print(f"Modo desconhecido: {mode}", file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
