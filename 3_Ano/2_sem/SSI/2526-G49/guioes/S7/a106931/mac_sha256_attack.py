import sys
import struct

# Length Extension Attack sobre prefix-MAC = SHA256(key || msg).
#
# O SHA256 usa Merkle-Damgård: após processar todos os blocos de (key||msg),
# o estado interno H0..H7 é exactamente o digest (o MAC). Um atacante que
# conheça MAC = SHA256(key||msg) pode continuar o cálculo a partir desse
# estado, sem conhecer key, e produzir um MAC válido para a mensagem estendida
# msg || padding_original || ext.

KEY_LEN = 32  # tamanho da chave (hipótese do atacante)

# ---------- SHA256 primitives ----------

def rotr32(x, n):
    return ((x >> n) | (x << (32 - n))) & 0xFFFFFFFF

_K = [
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2,
]

def _compress(state, block):
    W = list(struct.unpack('>16I', block))
    for i in range(16, 64):
        s0 = rotr32(W[i-15],7) ^ rotr32(W[i-15],18) ^ (W[i-15] >> 3)
        s1 = rotr32(W[i-2],17) ^ rotr32(W[i-2],19)  ^ (W[i-2]  >> 10)
        W.append((W[i-16] + s0 + W[i-7] + s1) & 0xFFFFFFFF)
    a, b, c, d, e, f, g, h = state
    for i in range(64):
        S1  = rotr32(e,6) ^ rotr32(e,11) ^ rotr32(e,25)
        ch  = (e & f) ^ ((e ^ 0xFFFFFFFF) & g)
        t1  = (h + S1 + ch + _K[i] + W[i]) & 0xFFFFFFFF
        S0  = rotr32(a,2) ^ rotr32(a,13) ^ rotr32(a,22)
        maj = (a & b) ^ (a & c) ^ (b & c)
        t2  = (S0 + maj) & 0xFFFFFFFF
        a, b, c, d, e, f, g, h = (t1+t2)&0xFFFFFFFF, a, b, c, (d+t1)&0xFFFFFFFF, e, f, g
    return tuple((s + x) & 0xFFFFFFFF for s, x in zip(state, (a,b,c,d,e,f,g,h)))

def _md_pad(msg_len_bytes):
    """SHA256 Merkle-Damgård padding para mensagem de msg_len_bytes bytes."""
    pad = b'\x80'
    while (msg_len_bytes + len(pad) + 8) % 64 != 0:
        pad += b'\x00'
    return pad + struct.pack('>Q', msg_len_bytes * 8)

# ---------- Length Extension ----------

def length_extend(mac_hex, msg, ext, key_len):
    """
    Dado MAC = SHA256(key || msg), forja um MAC válido para
    key || msg || padding_original || ext, sem conhecer key.

    Devolve (new_mac_hex, new_msg) onde new_msg é o que o verificador
    deve autenticar (msg || padding || ext).
    """
    # Estado interno após processar (key || msg || padding) = o MAC
    state = struct.unpack('>8I', bytes.fromhex(mac_hex))

    # Padding que foi adicionado internamente ao calcular SHA256(key || msg)
    inner_pad = _md_pad(key_len + len(msg))

    # Número de bytes já processados pelos blocos originais
    already = key_len + len(msg) + len(inner_pad)  # múltiplo de 64

    # Construir ext + padding com length counter correcto
    ext_total_len = already + len(ext)  # total de bytes "reais" incluindo ext
    ext_pad = b'\x80'
    while (already + len(ext) + len(ext_pad) + 8) % 64 != 0:
        ext_pad += b'\x00'
    ext_pad += struct.pack('>Q', ext_total_len * 8)

    # Processar os blocos de (ext || ext_pad) a partir do estado conhecido
    data = ext + ext_pad
    for i in range(0, len(data), 64):
        state = _compress(state, data[i:i+64])

    new_mac = struct.pack('>8I', *state).hex()
    new_msg = msg + inner_pad + ext      # mensagem que o verificador autentica
    return new_mac, new_msg

# ---------- Main ----------

if len(sys.argv) != 3:
    print("Uso: python mac_sha256_attack.py <fich> <ext>")
    sys.exit(1)

fich = sys.argv[1]
ext  = sys.argv[2].encode()

msg = open(fich, 'rb').read()
mac = open(fich + '.mac', 'rb').read().hex()

new_mac, new_msg = length_extend(mac, msg, ext, KEY_LEN)

with open(fich + '.ext', 'wb') as f:
    f.write(new_msg)

with open(fich + '.ext.mac', 'wb') as f:
    f.write(bytes.fromhex(new_mac))

print(f"Mensagem estendida guardada em '{fich}.ext'")
print(f"Novo MAC guardado em '{fich}.ext.mac'")
print(f"Padding adicionado: {len(msg + _md_pad(KEY_LEN + len(msg))) - len(msg)} bytes")
