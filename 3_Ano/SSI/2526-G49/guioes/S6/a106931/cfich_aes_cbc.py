import sys
import os
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.padding import PKCS7
from cryptography.hazmat.backends import default_backend

KEY_SIZE = 32  # AES-256
IV_SIZE  = 16  # AES block size

def setup(fkey):
    key = os.urandom(KEY_SIZE)
    with open(fkey, 'wb') as f:
        f.write(key)
    print(f"Chave AES-CBC gerada e guardada em '{fkey}'")

def enc(fich, fkey):
    key       = open(fkey, 'rb').read()
    plaintext = open(fich, 'rb').read()

    iv = os.urandom(IV_SIZE)

    # PKCS7 padding para que o plaintext seja múltiplo de 16 bytes
    padder = PKCS7(128).padder()
    padded = padder.update(plaintext) + padder.finalize()

    cipher    = Cipher(algorithms.AES(key), modes.CBC(iv), backend=default_backend())
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(padded) + encryptor.finalize()

    out = fich + '.enc'
    with open(out, 'wb') as f:
        f.write(iv + ciphertext)  # IV nos primeiros 16 bytes
    print(f"'{fich}' cifrado (AES-CBC) -> '{out}'")

def dec(fich, fkey):
    key  = open(fkey, 'rb').read()
    data = open(fich, 'rb').read()

    iv         = data[:IV_SIZE]
    ciphertext = data[IV_SIZE:]

    cipher    = Cipher(algorithms.AES(key), modes.CBC(iv), backend=default_backend())
    decryptor = cipher.decryptor()
    padded    = decryptor.update(ciphertext) + decryptor.finalize()

    unpadder  = PKCS7(128).unpadder()
    plaintext = unpadder.update(padded) + unpadder.finalize()

    out = fich + '.dec'
    with open(out, 'wb') as f:
        f.write(plaintext)
    print(f"'{fich}' decifrado (AES-CBC) -> '{out}'")

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
