import sys
import random

def main():
    if len(sys.argv) < 4:
        print(f"Uso: {sys.argv[0]} <tam_chave> <ctxtfile> <palavra1> [palavra2 ...]", file=sys.stderr)
        sys.exit(1)

    key_len  = int(sys.argv[1])
    ctxtfile = sys.argv[2]
    words    = sys.argv[3:]

    with open(ctxtfile, 'rb') as f:
        ctxt = f.read()

    # O bad_prng usa random.seed(random.randbytes(2)):
    # a semente tem apenas 2 bytes -> 2^16 = 65536 possibilidades.
    # Para cada semente possível, geramos a chave correspondente
    # e verificamos se o texto-limpo contém uma das palavras fornecidas.
    for seed_val in range(65536):
        seed_bytes = seed_val.to_bytes(2, 'big')
        random.seed(seed_bytes)
        key = random.randbytes(key_len)

        plain_bytes = bytes(c ^ k for c, k in zip(ctxt, key))
        try:
            plaintext = plain_bytes.decode('utf-8', errors='replace')
        except Exception:
            continue

        if any(word in plaintext for word in words):
            print(plaintext, end='')
            return

if __name__ == '__main__':
    main()
