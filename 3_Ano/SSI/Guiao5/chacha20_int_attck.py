import sys

if len(sys.argv) != 5:
    print("Uso: python chacha20_int_attck.py <fctxt> <pos> <ptxtAtPos> <newPtxtAtPos>")
    sys.exit(1)

fctxt = sys.argv[1]
pos = int(sys.argv[2])
ptxt = sys.argv[3].encode()
new_ptxt = sys.argv[4].encode()

if len(ptxt) != len(new_ptxt):
    print("ptxtAtPos e newPtxtAtPos devem ter o mesmo tamanho")
    sys.exit(1)

# ler criptograma
with open(fctxt, "rb") as f:
    data = bytearray(f.read())

# ChaCha20 guarda nonce nos primeiros 16 bytes
offset = 16 + pos

for i in range(len(ptxt)):
    data[offset+i] ^= ptxt[i] ^ new_ptxt[i]

# guardar criptograma manipulado
out = fctxt + ".attck"

with open(out, "wb") as f:
    f.write(data)

print("Criptograma manipulado guardado em:", out)