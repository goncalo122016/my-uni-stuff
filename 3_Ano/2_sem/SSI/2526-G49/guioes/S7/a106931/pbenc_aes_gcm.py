import sys
import os
import getpass
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.backends import default_backend

KEY_SIZE   = 32  # AES-256-GCM
NONCE_SIZE = 12  # 96 bits — tamanho recomendado para GCM
SALT_SIZE  = 16
ITERATIONS = 200000

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

    aesgcm     = AESGCM(key)
    ciphertext = aesgcm.encrypt(nonce, plaintext, None)
    # ciphertext inclui automaticamente o GCM authentication tag (16 bytes no fim)

    out = fich + '.enc'
    with open(out, 'wb') as f:
        f.write(salt + nonce + ciphertext)  # [salt 16B | nonce 12B | ctxt+tag]
    print(f"'{fich}' cifrado (AES-GCM) -> '{out}'")

def dec(fich):
    password = getpass.getpass("Passphrase: ").encode()
    data     = open(fich, 'rb').read()

    salt       = data[:SALT_SIZE]
    nonce      = data[SALT_SIZE:SALT_SIZE + NONCE_SIZE]
    ciphertext = data[SALT_SIZE + NONCE_SIZE:]
    key        = derive_key(password, salt)

    aesgcm = AESGCM(key)
    try:
        plaintext = aesgcm.decrypt(nonce, ciphertext, None)
    except Exception:
        print("Erro: autenticação falhou! Ficheiro corrompido ou passphrase errada.", file=sys.stderr)
        sys.exit(1)

    out = fich + '.dec'
    with open(out, 'wb') as f:
        f.write(plaintext)
    print(f"'{fich}' decifrado (AES-GCM) -> '{out}'")

def main():
    if len(sys.argv) != 3:
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
