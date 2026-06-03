import sys

if len(sys.argv) != 4:
    print("Uso: python flip_bit.py <ficheiro> <byte_pos> <bit_pos>")
    print("bit_pos: 0-7")
    sys.exit(1)

filename = sys.argv[1]
byte_pos = int(sys.argv[2])
bit_pos = int(sys.argv[3])

# ler ficheiro
with open(filename, "rb") as f:
    data = bytearray(f.read())

# inverter bit
data[byte_pos] ^= (1 << bit_pos)

# guardar novo ficheiro
out = filename + ".mod"

with open(out, "wb") as f:
    f.write(data)

print("Bit alterado!")
print("Novo ficheiro:", out)