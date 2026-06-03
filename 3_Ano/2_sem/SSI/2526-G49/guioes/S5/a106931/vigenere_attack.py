import sys
from itertools import product

# Letras mais frequentes em português (por ordem decrescente)
PT_FREQ = 'AEOSRINDMUTCLPVGHKBFZJXQWY'

def preproc(s):
    return ''.join(c.upper() for c in s if c.isalpha())

def vigenere_dec(text, key):
    result = []
    for i, c in enumerate(text):
        shift = ord(key[i % len(key)]) - ord('A')
        result.append(chr((ord(c) - ord('A') - shift) % 26 + ord('A')))
    return ''.join(result)

def score_shift(slice_text, shift):
    """Pontua um shift pelo quão bem as letras decifradas correspondem à frequência portuguesa."""
    score = 0
    for c in slice_text:
        decrypted = chr((ord(c) - ord('A') - shift) % 26 + ord('A'))
        # Letras mais frequentes recebem pontuação maior
        if decrypted in PT_FREQ:
            score += 26 - PT_FREQ.index(decrypted)
    return score

def main():
    if len(sys.argv) < 4:
        print(f"Uso: {sys.argv[0]} <tam_chave> <criptograma> <palavra1> [palavra2 ...]", file=sys.stderr)
        sys.exit(1)

    key_len    = int(sys.argv[1])
    ciphertext = preproc(sys.argv[2])
    words      = [preproc(w) for w in sys.argv[3:]]

    # Dividir o criptograma em key_len fatias (cada uma cifrada por um César diferente)
    slices = [''.join(ciphertext[i::key_len]) for i in range(key_len)]

    # Para cada fatia, ordenar os 26 shifts por pontuação de frequência
    # Guardar os top-5 candidatos por posição
    top_k = 5
    candidates = []
    for sl in slices:
        ranked = sorted(range(26), key=lambda s: score_shift(sl, s), reverse=True)
        candidates.append(ranked[:top_k])

    # Experimentar todas as combinações de candidatos
    for combo in product(*candidates):
        key = ''.join(chr(ord('A') + s) for s in combo)
        plaintext = vigenere_dec(ciphertext, key)
        for word in words:
            if word in plaintext:
                print(key)
                print(plaintext)
                return

    # Fallback: brute-force completo se os top-k não encontraram solução
    for combo in product(range(26), repeat=key_len):
        key = ''.join(chr(ord('A') + s) for s in combo)
        plaintext = vigenere_dec(ciphertext, key)
        for word in words:
            if word in plaintext:
                print(key)
                print(plaintext)
                return

if __name__ == '__main__':
    main()
