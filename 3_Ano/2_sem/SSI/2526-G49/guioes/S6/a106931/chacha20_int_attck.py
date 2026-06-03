import sys

# Ataque de integridade a ChaCha20 (stream cipher).
#
# Princípio: ChaCha20 cifra por XOR com um keystream.
#   ciphertext[i] = plaintext[i] XOR keystream[i]
#
# Se soubermos que em <pos> está <ptxtAtPos>, podemos substituí-lo por
# <newPtxtAtPos> sem conhecer a chave, usando a propriedade do XOR:
#   new_ctxt[i] = ctxt[i] XOR ptxt[i] XOR new_ptxt[i]
#
# O ficheiro .enc tem: [16 bytes NONCE] + [ciphertext]
# Por isso o offset real no ficheiro é: NONCE_SIZE + pos

NONCE_SIZE = 16

if len(sys.argv) != 5:
    print("Uso: python chacha20_int_attck.py <fctxt> <pos> <ptxtAtPos> <newPtxtAtPos>")
    sys.exit(1)

fctxt    = sys.argv[1]
pos      = int(sys.argv[2])
ptxt     = sys.argv[3].encode()
new_ptxt = sys.argv[4].encode()

if len(ptxt) != len(new_ptxt):
    print("Erro: ptxtAtPos e newPtxtAtPos devem ter o mesmo tamanho", file=sys.stderr)
    sys.exit(1)

data = bytearray(open(fctxt, 'rb').read())

offset = NONCE_SIZE + pos
for i in range(len(ptxt)):
    data[offset + i] ^= ptxt[i] ^ new_ptxt[i]

out = fctxt + '.attck'
with open(out, 'wb') as f:
    f.write(data)

print(f"Criptograma manipulado guardado em '{out}'")
