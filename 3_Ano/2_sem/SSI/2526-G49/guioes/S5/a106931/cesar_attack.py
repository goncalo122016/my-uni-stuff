import sys

def preproc(s):
    return ''.join(c.upper() for c in s if c.isalpha())

def cesar_dec(text, shift):
    return ''.join(chr((ord(c) - ord('A') - shift) % 26 + ord('A')) for c in text)

def main():
    if len(sys.argv) < 3:
        print(f"Uso: {sys.argv[0]} <criptograma> <palavra1> [palavra2 ...]", file=sys.stderr)
        sys.exit(1)

    ciphertext = preproc(sys.argv[1])
    words = [preproc(w) for w in sys.argv[2:]]

    for shift in range(26):
        plaintext = cesar_dec(ciphertext, shift)
        for word in words:
            if word in plaintext:
                print(chr(ord('A') + shift))
                print(plaintext)
                return

if __name__ == '__main__':
    main()
