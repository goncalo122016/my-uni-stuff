import sys
import os
import getpass
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
from cryptography.hazmat.primitives import hashes, hmac
from cryptography.hazmat.backends import default_backend
from cryptography.exceptions import InvalidSignature

KEY_SIZE     = 32   # AES-256
MAC_KEY_SIZE = 32   # HMAC-SHA256
NONCE_SIZE   = 16   # AES-CTR nonce
SALT_SIZE    = 16   # PBKDF2 salt
ITERATIONS   = 200000

def derive_keys(password: bytes, salt: bytes):
    # Derivar chave de cifra e chave de MAC com uma só chamada à KDF
    kdf = PBKDF2HMAC(
        algorithm=hashes.SHA256(),
        length=KEY_SIZE + MAC_KEY_SIZE,
        salt=salt,
        iterations=ITERATIONS,
        backend=default_backend()
    )
    material = kdf.derive(password)
    return material[:KEY_SIZE], material[KEY_SIZE:]

def compute_hmac(key_mac: bytes, data: bytes) -> bytes:
    h = hmac.HMAC(key_mac, hashes.SHA256(), backend=default_backend())
    h.update(data)
    return h.finalize()

def verify_hmac(key_mac: bytes, data: bytes, tag: bytes) -> bool:
    h = hmac.HMAC(key_mac, hashes.SHA256(), backend=default_backend())
    h.update(data)
    try:
        h.verify(tag)
        return True
    except InvalidSignature:
        return False

def enc(fich):
    password  = getpass.getpass("Passphrase: ").encode()
    plaintext = open(fich, 'rb').read()

    salt          = os.urandom(SALT_SIZE)
    nonce         = os.urandom(NONCE_SIZE)
    key_enc, key_mac = derive_keys(password, salt)

    cipher    = Cipher(algorithms.AES(key_enc), modes.CTR(nonce), backend=default_backend())
    encryptor = cipher.encryptor()
    ciphertext = encryptor.update(plaintext) + encryptor.finalize()

    # Encrypt-then-MAC: o MAC cobre salt + nonce + ciphertext
    authenticated = salt + nonce + ciphertext
    tag = compute_hmac(key_mac, authenticated)

    out = fich + '.enc'
    with open(out, 'wb') as f:
        f.write(salt + nonce + ciphertext + tag)  # [salt|nonce|ctxt|hmac]
    print(f"'{fich}' cifrado (AES-CTR + HMAC) -> '{out}'")

def dec(fich):
    password = getpass.getpass("Passphrase: ").encode()
    data     = open(fich, 'rb').read()

    salt       = data[:SALT_SIZE]
    nonce      = data[SALT_SIZE:SALT_SIZE + NONCE_SIZE]
    ciphertext = data[SALT_SIZE + NONCE_SIZE:-MAC_KEY_SIZE]
    tag        = data[-MAC_KEY_SIZE:]

    key_enc, key_mac = derive_keys(password, salt)

    # Verificar MAC ANTES de decifrar (encrypt-then-MAC)
    authenticated = salt + nonce + ciphertext
    if not verify_hmac(key_mac, authenticated, tag):
        print("Erro: MAC inválido! Ficheiro corrompido ou passphrase errada.", file=sys.stderr)
        sys.exit(1)

    cipher    = Cipher(algorithms.AES(key_enc), modes.CTR(nonce), backend=default_backend())
    decryptor = cipher.decryptor()
    plaintext = decryptor.update(ciphertext) + decryptor.finalize()

    out = fich + '.dec'
    with open(out, 'wb') as f:
        f.write(plaintext)
    print(f"'{fich}' decifrado (AES-CTR + HMAC) -> '{out}'")

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
