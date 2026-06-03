import sys
import os
import getpass
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.backends import default_backend

KEY_SIZE   = 32     # 256 bits
NONCE_SIZE = 16     # ChaCha20 nonce
SALT_SIZE  = 16     # salt para PBKDF2
ITERATIONS = 200000 # iterações PBKDF2 (NIST recomenda >= 100000)

def derive_key(password: bytes, salt: bytes) -> bytes:
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=KEY_SIZE,
        salt=salt,
        iterations=ITERATIONS,
        backend=default_backend()
    )
    return kdf.derive(password)

def enc(fich):
    password  = getpass.getpass("Passphrase: ").encode()
    plaintext = open(fich, 'rb').read()

    salt  = os.urandom(SALT_SIZE)
    nonce = os.urandom(NONCE_SIZE)
    key   = derive_key(password, salt)

    cipher    = Cipher(algorithms.ChaCha20(key, nonce), mode=None, backend=default_backend())
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    # Formato do ficheiro: [salt 16B] [nonce 16B] [ciphertext]
    out = fich + '.enc'
    with open(out, 'wb') as f:
        f.write(salt + nonce + ciphertext)
    print(f"'{fich}' cifrado (PBE+ChaCha20) -> '{out}'")

def dec(fich):
    password = getpass.getpass("Passphrase: ").encode()
    data     = open(fich, 'rb').read()

    salt       = data[:SALT_SIZE]
    nonce      = data[SALT_SIZE:SALT_SIZE + NONCE_SIZE]
    ciphertext = data[SALT_SIZE + NONCE_SIZE:]
    key        = derive_key(password, salt)

    cipher    = Cipher(algorithms.ChaCha20(key, nonce), mode=None, backend=default_backend())
    decryptor = cipher.decryptor()
    plaintext = decryptor.update(ciphertext) + decryptor.finalize()

    out = fich + '.dec'
    with open(out, 'wb') as f:
        f.write(plaintext)
    print(f"'{fich}' decifrado (PBE+ChaCha20) -> '{out}'")

def main():
    if len(sys.argv) < 3:
        print("Uso:")
        print("  enc <fich>")
        print("  dec <fich>")
        sys.exit(1)

    op = sys.argv[1]
    if op == 'enc':
        enc(sys.argv[2])
    elif op == 'dec':
        dec(sys.argv[2])
    else:
        print(f"Operação desconhecida: {op}", file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
