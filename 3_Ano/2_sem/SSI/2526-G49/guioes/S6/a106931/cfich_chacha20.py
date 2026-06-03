import sys
import os
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms
from cryptography.hazmat.backends import default_backend

KEY_SIZE   = 32  # 256 bits
NONCE_SIZE = 16  # 128 bits

def setup(fkey):
    key = os.urandom(KEY_SIZE)
    with open(fkey, 'wb') as f:
        f.write(key)
    print(f"Chave gerada e guardada em '{fkey}'")

def enc(fich, fkey):
    key       = open(fkey, 'rb').read()
    plaintext = open(fich, 'rb').read()

    nonce = os.urandom(NONCE_SIZE)  # NONCE aleatório por cada cifra

    cipher    = Cipher(algorithms.ChaCha20(key, nonce), mode=None, backend=default_backend())
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    out = fich + '.enc'
    with open(out, 'wb') as f:
        f.write(nonce + ciphertext)  # guardar NONCE junto com o criptograma
    print(f"'{fich}' cifrado -> '{out}'")

def dec(fich, fkey):
    key  = open(fkey, 'rb').read()
    data = open(fich, 'rb').read()

    nonce      = data[:NONCE_SIZE]
    ciphertext = data[NONCE_SIZE:]

    cipher    = Cipher(algorithms.ChaCha20(key, nonce), mode=None, backend=default_backend())
    decryptor = cipher.decryptor()
    plaintext = decryptor.update(ciphertext) + decryptor.finalize()

    out = fich + '.dec'
    with open(out, 'wb') as f:
        f.write(plaintext)
    print(f"'{fich}' decifrado -> '{out}'")

def main():
    if len(sys.argv) < 3:
        print("Uso:")
        print("  setup <fkey>")
        print("  enc   <fich> <fkey>")
        print("  dec   <fich> <fkey>")
        sys.exit(1)

    op = sys.argv[1]
    if op == 'setup':
        setup(sys.argv[2])
    elif op == 'enc':
        enc(sys.argv[2], sys.argv[3])
    elif op == 'dec':
        dec(sys.argv[2], sys.argv[3])
    else:
        print(f"Operação desconhecida: {op}", file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
