import sys
import hashpumpy

if len(sys.argv) != 3:
    print("Uso: python mac_sha256_attack.py <fich> <ext>")
    sys.exit(1)

fich = sys.argv[1]
extension = sys.argv[2].encode()

# ler mensagem original
with open(fich, "rb") as f:
    msg = f.read()

# ler MAC original
with open(fich + ".mac", "rb") as f:
    mac = f.read().hex()

# tamanho da chave (temos de adivinhar)
key_len = 32

# ataque
new_mac, new_msg = hashpumpy.hashpump(mac, msg, extension, key_len)

# guardar nova mensagem
with open(fich + ".ext", "wb") as f:
    f.write(new_msg)

# guardar novo MAC
with open(fich + ".ext.mac", "wb") as f:
    f.write(bytes.fromhex(new_mac))

print("Mensagem estendida guardada em:", fich + ".ext")
print("Novo MAC guardado em:", fich + ".ext.mac")