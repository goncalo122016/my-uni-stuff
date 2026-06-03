import sys
import os
from cryptography.hazmat.primitives import hashes

KEY_SIZE = 32  # 256 bits

def setup(fkey):
    key = os.urandom(KEY_SIZE)
    with open(fkey, 'wb') as f:
        f.write(key)
    print(f"Chave gerada e guardada em '{fkey}'")

def compute_mac(key: bytes, data: bytes) -> bytes:
    # prefix-MAC: H(k || m)
    h = hashes.Hash(hashes.SHA256())
    h.update(key + data)
    return h.finalize()

def mac(fich, fkey):
    key  = open(fkey, 'rb').read()
    data = open(fich, 'rb').read()
    tag  = compute_mac(key, data)
    out  = fich + '.mac'
    with open(out, 'wb') as f:
        f.write(tag)
    print(f"MAC de '{fich}' guardado em '{out}'")

def ver(fich, fkey):
    key      = open(fkey, 'rb').read()
    data     = open(fich, 'rb').read()
    expected = open(fich + '.mac', 'rb').read()
    computed = compute_mac(key, data)
    print(computed == expected)

def main():
    if len(sys.argv) < 3:
        print("Uso:")
        print("  setup <fkey>")
        print("  mac   <fich> <fkey>")
        print("  ver   <fich> <fkey>")
        sys.exit(1)

    op = sys.argv[1]
    if op == 'setup':
        setup(sys.argv[2])
    elif op == 'mac':
        mac(sys.argv[2], sys.argv[3])
    elif op == 'ver':
        ver(sys.argv[2], sys.argv[3])
    else:
        print(f"Operação desconhecida: {op}", file=sys.stderr)
        sys.exit(1)

if __name__ == '__main__':
    main()
