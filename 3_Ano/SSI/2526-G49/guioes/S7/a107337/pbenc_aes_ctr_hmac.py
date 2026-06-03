import sys
import os
import getpass
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives import hashes, hmac

KEY_SIZE = 32   # 256 bits
MAC_KEY_SIZE = 32
NONCE_SIZE = 16
SALT_SIZE = 16
ITERATIONS = 100000

def derive_key(password, salt):
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=KEY_SIZE + MAC_KEY_SIZE,
        salt=salt,
        iterations=ITERATIONS,
        backend=default_backend()
    )
    result = kdf.derive(password)
    return result[:KEY_SIZE], result[KEY_SIZE:]

def get_password():
    return getpass.getpass("Passphrase: ").encode()

def enc(fich):
    password = get_password()

    salt = os.urandom(SALT_SIZE)
    key_enc, key_mac = derive_key(password, salt)

    with open(fich, "rb") as f:
        plaintext = f.read()

    nonce = os.urandom(NONCE_SIZE)
    
    cipher = Cipher(
        algorithms.AES(key_enc),
        modes.CTR(nonce),
        backend=default_backend()
    )

    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    # HMAC (Encrypt-then-MAC)
    h = hmac.HMAC(key_mac, hashes.SHA256(), backend=default_backend())
    h.update(salt + nonce + ciphertext)
    tag = h.finalize()

    out_file = f"{fich}.enc"
    with open(out_file, "wb") as f:
        f.write(salt + nonce + ciphertext + tag)

    print(f"Ficheiro {fich} encriptado e salvo como {out_file}")

def dec(fich):
    password = get_password()

    with open(fich, "rb") as f:
        data = f.read()

    salt = data[:SALT_SIZE]
    nonce = data[SALT_SIZE:SALT_SIZE + NONCE_SIZE]
    ciphertext = data[SALT_SIZE + NONCE_SIZE:-32]
    tag = data[-32:]

    h = hmac.HMAC(key_mac, hashes.SHA256(), backend=default_backend())
    h.update(salt + nonce + ciphertext)
    try:
        h.verify(tag)
    except Exception as e:
        print("Tag de autenticação inválida!")
        sys.exit(1)

    key_enc, key_mac = derive_key(password, salt)

    cipher = Cipher(
        algorithms.AES(key_enc),
        modes.CTR(nonce),
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