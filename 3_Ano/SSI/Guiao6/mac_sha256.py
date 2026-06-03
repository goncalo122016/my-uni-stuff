import sys
import os
from cryptography.hazmat.primitives import hashes

KEY_SIZE = 32   # 256 bits
NONCE_SIZE = 16

def setup(fkey):
    key = os.urandom(KEY_SIZE)

    with open(fkey, "wb") as f:
        f.write(key)
    
    print(f"Chave gerada e salva em {fkey}")

def mac(fich, fkey):
    with open(fkey, "rb") as f:
        key = f.read()

    with open(fich, "rb") as f:
        plaintext = f.read()

    digest = hashes.Hash(hashes.SHA256())
    digest.update(key + plaintext)
    hash = digest.finalize()

    out_file = f"{fich}.mac"
    with open(out_file, "wb") as f:
        f.write(hash)

    print(f"Ficheiro {fich} processado e MAC salvo como {out_file}")

def ver(fich, fkey):
    with open(fkey, "rb") as f:
        key = f.read()

    with open(fich, "rb") as f:
        data = f.read()

    with open(fich + ".mac", "rb") as f:
        expected_mac = f.read()

    digest = hashes.Hash(hashes.SHA256())
    digest.update(key + data)
    computed_hash = digest.finalize()

    print(expected_mac == computed_hash)

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

    elif op == "mac":
        mac(sys.argv[2], sys.argv[3])

    elif op == "ver":
        ver(sys.argv[2], sys.argv[3])

    else:
        print("Operação inválida")


if __name__ == "__main__":
    main()