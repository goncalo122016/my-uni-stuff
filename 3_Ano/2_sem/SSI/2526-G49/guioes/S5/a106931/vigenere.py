import sys

def preproc(s):
    return ''.join(c.upper() for c in s if c.isalpha())

def vigenere_enc(text, key):
    result = []
    for i, c in enumerate(text):
        shift = ord(key[i % len(key)]) - ord('A')
        result.append(chr((ord(c) - ord('A') + shift) % 26 + ord('A')))
    return ''.join(result)

def vigenere_dec(text, key):
    result = []
    for i, c in enumerate(text):
        shift = ord(key[i % len(key)]) - ord('A')
        result.append(chr((ord(c) - ord('A') - shift) % 26 + ord('A')))
    return ''.join(result)

def main():
    if len(sys.argv) != 4:
        print(f"Uso: {sys.argv[0]} enc|dec <chave> <mensagem>", file=sys.stderr)
        sys.exit(1)

    mode = sys.argv[1]
    key  = preproc(sys.argv[2])
    text = preproc(sys.argv[3])

    if mode == 'enc':
        print(vigenere_enc(text, key))
    elif mode == 'dec':
        print(vigenere_dec(text, key))
    else:
        print(f"Modo desconhecido: {mode}", file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
