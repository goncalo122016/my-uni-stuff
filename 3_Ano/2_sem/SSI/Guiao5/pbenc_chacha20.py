import sys
import os
import getpass
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives import hashes

KEY_SIZE = 32   # 256 bits
NONCE_SIZE = 16
SALT_SIZE = 16
ITERATIONS = 100000

def derive_key(password, salt):
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=KEY_SIZE,
        salt=salt,
        iterations=ITERATIONS,
        backend=default_backend()
    )
    return kdf.derive(password)

def get_password():
    return getpass.getpass("Passphrase: ").encode()

def enc(fich):
    password = get_password()

    salt = os.urandom(SALT_SIZE)
    key = derive_key(password, salt)

    with open(fich, "rb") as f:
        plaintext = f.read()

    nonce = os.urandom(NONCE_SIZE)
    
    cipher = Cipher(
        algorithms.ChaCha20(key, nonce),
        mode=None,
        backend=default_backend()
    )
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    out_file = f"{fich}.enc"
    with open(out_file, "wb") as f:
        f.write(salt + nonce + ciphertext)

    print(f"Ficheiro {fich} encriptado e salvo como {out_file}")

def dec(fich):
    password = get_password()

    with open(fich, "rb") as f:
        data = f.read()

    salt = data[:SALT_SIZE]
    nonce = data[SALT_SIZE:SALT_SIZE + NONCE_SIZE]
    ciphertext = data[SALT_SIZE + NONCE_SIZE:]

    key = derive_key(password, salt)

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
        print(" enc <fich> <fkey>")
        print(" dec <fich> <fkey>")
        sys.exit(1)

    op = sys.argv[1]

    if op == "enc":
        enc(sys.argv[2])

    elif op == "dec":
        dec(sys.argv[2])

    else:
        print("Operação inválida")

if __name__ == "__main__":
    main()