import sys
import os
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend

KEY_SIZE  = 32  # AES-256
NONCE_SIZE = 16  # AES block size (usado como nonce/IV em CTR)

def setup(fkey):
    key = os.urandom(KEY_SIZE)
    with open(fkey, 'wb') as f:
        f.write(key)
    print(f"Chave AES-CTR gerada e guardada em '{fkey}'")

def enc(fich, fkey):
    key       = open(fkey, 'rb').read()
    plaintext = open(fich, 'rb').read()

    nonce = os.urandom(NONCE_SIZE)  # CTR não necessita padding (modo stream)

    cipher    = Cipher(algorithms.AES(key), modes.CTR(nonce), backend=default_backend())
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    out = fich + '.enc'
    with open(out, 'wb') as f:
        f.write(nonce + ciphertext)
    print(f"'{fich}' cifrado (AES-CTR) -> '{out}'")

def dec(fich, fkey):
    key  = open(fkey, 'rb').read()
    data = open(fich, 'rb').read()

    nonce      = data[:NONCE_SIZE]
    ciphertext = data[NONCE_SIZE:]

    cipher    = Cipher(algorithms.AES(key), modes.CTR(nonce), backend=default_backend())
    decryptor = cipher.decryptor()
    plaintext = decryptor.update(ciphertext) + decryptor.finalize()

    out = fich + '.dec'
    with open(out, 'wb') as f:
        f.write(plaintext)
    print(f"'{fich}' decifrado (AES-CTR) -> '{out}'")

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
