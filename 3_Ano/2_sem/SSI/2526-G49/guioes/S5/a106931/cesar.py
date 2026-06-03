import sys

def preproc(s):
    return ''.join(c.upper() for c in s if c.isalpha())

def cesar_enc(text, shift):
    return ''.join(chr((ord(c) - ord('A') + shift) % 26 + ord('A')) for c in text)

def cesar_dec(text, shift):
    return ''.join(chr((ord(c) - ord('A') - shift) % 26 + ord('A')) for c in text)

def main():
    if len(sys.argv) != 4:
        print(f"Uso: {sys.argv[0]} enc|dec <chave> <mensagem>", file=sys.stderr)
        sys.exit(1)

    mode  = sys.argv[1]
    shift = ord(sys.argv[2].upper()) - ord('A')
    text  = preproc(sys.argv[3])

    if mode == 'enc':
        print(cesar_enc(text, shift))
    elif mode == 'dec':
        print(cesar_dec(text, shift))
    else:
        print(f"Modo desconhecido: {mode}", file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
