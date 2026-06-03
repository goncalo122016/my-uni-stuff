import sys
import os
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms
from cryptography.hazmat.backends import default_backend

KEY_SIZE = 32   # 256 bits
NONCE_SIZE = 16

def setup(fkey):
    key = os.urandom(KEY_SIZE)

    with open(fkey, "wb") as f:
        f.write(key)
    
    print(f"Chave gerada e salva em {fkey}")

def enc(fich, fkey):
    with open(fkey, "rb") as f:
        key = f.read()

    with open(fich, "rb") as f:
        plaintext = f.read()

    # nonce = os.urandom(NONCE_SIZE)
    nonce = b'\x00' * 16

    cipher = Cipher(
        algorithms.ChaCha20(key, nonce),
        mode=None,
        backend=default_backend()
    )
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    out_file = f"{fich}.enc"
    with open(out_file, "wb") as f:
        f.write(nonce + ciphertext)

    print(f"Ficheiro {fich} encriptado e salvo como {out_file}")

def dec(fich, fkey):
    with open(fkey, "rb") as f:
        key = f.read()

    with open(fich, "rb") as f:
        data = f.read()

    nonce = data[:NONCE_SIZE]
    ciphertext = data[NONCE_SIZE:]

    cipher = Cipher(
        algorithms.ChaCha20(key, nonce),
        mode=None,
        backend=default_backend()
    )
    decryptor = cipher.decryptor()
    plaintext = decryptor.update(ciphertext) + decryptor.finalize()

    out_file = f"{fich}.dec"
    with open(out_file, "wb") as f:
        f.write(plaintext)

    print(f"Ficheiro {fich} desencriptado e salvo como {out_file}")

def main():
    if len(sys.argv) < 3:
        print("Uso:")
        print(" setup <fkey>")
        print(" enc <fich> <fkey>")
        print(" dec <fich> <fkey>")
        sys.exit(1)

    op = sys.argv[1]

    if op == "setup":
        setup(sys.argv[2])

    elif op == "enc":
        enc(sys.argv[2], sys.argv[3])

    elif op == "dec":
        dec(sys.argv[2], sys.argv[3])

    else:
        print("Operação inválida")


if __name__ == "__main__":
    main()