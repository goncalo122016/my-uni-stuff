import os
from multiprocessing import Process, Pipe
from cryptography.hazmat.primitives.asymmetric import dh, padding as asym_padding
from cryptography.hazmat.primitives import serialization, hashes
from cryptography.hazmat.primitives.kdf.hkdf import HKDF
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography import x509
from cryptography.x509 import load_pem_x509_certificate

# ---- Parâmetros DH ----
p = int((
    "FFFFFFFF FFFFFFFF C90FDAA2 2168C234 C4C6628B"
    "80DC1CD1 29024E08 8A67CC74 020BBEA6 3B139B22"
    "514A0879 8E3404DD EF9519B3 CD3A431B 302B0A6D"
    "F25F1437 4FE1356D 6D51C245 E485B576 625E7EC6"
    "F44C42E9 A637ED6B 0BFF5CB6 F406B7ED EE386BFB"
    "5A899FA5 AE9F2411 7C4B1FE6 49286651 ECE45B3D"
    "C2007CB8 A163BF05 98DA4836 1C55D39A 69163FA8"
    "FD24CF5F 83655D23 DCA3AD96 1C62F356 208552BB"
    "9ED52907 7096966D 670C354E 4ABC9804 F1746C08"
    "CA18217C 32905E46 2E36CE3B E39E772C 180E8603"
    "9B2783A2 EC07A28F B5C55DF0 6F4C52C9 DE2BCBF6"
    "95581718 3995497C EA956AE5 15D22618 98FA0510"
    "15728E5A 8AACAA68 FFFFFFFF FFFFFFFF"
).replace(" ", ""), 16)
g = 2
parameters = dh.DHParameterNumbers(p, g).parameters()

NONCE_SIZE = 12
PSS_PADDING = asym_padding.PSS(
    mgf=asym_padding.MGF1(hashes.SHA256()),
    salt_length=asym_padding.PSS.MAX_LENGTH
)

# ---- Utilitários de serialização ----
def mkpair(x: bytes, y: bytes) -> bytes:
    """Serializa o par (x, y) precedendo x do seu tamanho em 2 bytes little-endian."""
    return len(x).to_bytes(2, "little") + x + y

def unpair(xy: bytes):
    """Recupera (x, y) de um par serializado com mkpair."""
    n = int.from_bytes(xy[:2], "little")
    return xy[2:2 + n], xy[2 + n:]

def load_privkey(path):
    return serialization.load_pem_private_key(open(path, "rb").read(), password=None)

def load_cert(path):
    return load_pem_x509_certificate(open(path, "rb").read())

def verify_cert(cert, ca_cert):
    """Verifica que cert foi assinado pela CA."""
    ca_pub = ca_cert.public_key()
    ca_pub.verify(
        cert.signature,
        cert.tbs_certificate_bytes,
        PSS_PADDING,
        cert.signature_hash_algorithm
    )

def sign(privkey, data: bytes) -> bytes:
    return privkey.sign(data, PSS_PADDING, hashes.SHA256())

def verify_sig(pubkey, sig: bytes, data: bytes):
    pubkey.verify(sig, data, PSS_PADDING, hashes.SHA256())

def derive_key(shared: bytes) -> bytes:
    return HKDF(
        algorithm=hashes.SHA256(), length=32,
        salt=None, info=b'sts-aes-gcm'
    ).derive(shared)

# ---- Processo Alice ----
def alice_process(conn):
    alice_priv_dh = parameters.generate_private_key()
    gx = alice_priv_dh.public_key().public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo
    )

    # 1. Alice → Bob: g^x
    conn.send(gx)

    # 2. Bob → Alice: g^y || SigB(g^y || g^x) || CertB
    msg = conn.recv()
    part1, certB_bytes = unpair(msg)
    gy_bytes, sigB     = unpair(part1)

    # Verificar CertB contra CA
    ca_cert   = load_cert("CA.crt")
    certB     = load_pem_x509_certificate(certB_bytes)
    try:
        verify_cert(certB, ca_cert)
        print("[Alice] CertB verificado pela CA ✓")
    except Exception as e:
        print(f"[Alice] ERRO: CertB inválido: {e}")
        return

    # Verificar assinatura do Bob: SigB(g^y || g^x)
    try:
        verify_sig(certB.public_key(), sigB, gy_bytes + gx)
        print("[Alice] Assinatura do Bob verificada ✓")
    except Exception as e:
        print(f"[Alice] ERRO: assinatura Bob inválida: {e}")
        return

    # 3. Alice → Bob: SigA(g^x || g^y) || CertA
    alice_priv_rsa = load_privkey("Alice.key")
    certA_bytes    = open("Alice.crt", "rb").read()
    sigA           = sign(alice_priv_rsa, gx + gy_bytes)
    conn.send(mkpair(sigA, certA_bytes))

    # 4. Calcular K e derivar chave
    bob_pub_dh = serialization.load_pem_public_key(gy_bytes)
    key        = derive_key(alice_priv_dh.exchange(bob_pub_dh))
    print(f"[Alice] chave AES-GCM: {key.hex()}")

    # 5. Enviar mensagem cifrada
    plaintext  = b"Ola Bob! Mensagem autenticada via Station-to-Station."
    nonce      = os.urandom(NONCE_SIZE)
    ciphertext = AESGCM(key).encrypt(nonce, plaintext, None)
    conn.send(nonce + ciphertext)
    print(f"[Alice] enviou: {plaintext.decode()}")

    # 6. Receber resposta do Bob
    data  = conn.recv()
    reply = AESGCM(key).decrypt(data[:NONCE_SIZE], data[NONCE_SIZE:], None)
    print(f"[Alice] resposta do Bob: {reply.decode()}")

# ---- Processo Bob ----
def bob_process(conn):
    bob_priv_dh = parameters.generate_private_key()
    gy = bob_priv_dh.public_key().public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo
    )

    # 1. Alice → Bob: g^x
    gx = conn.recv()

    # 2. Bob → Alice: g^y || SigB(g^y || g^x) || CertB
    bob_priv_rsa = load_privkey("Bob.key")
    certB_bytes  = open("Bob.crt", "rb").read()
    sigB         = sign(bob_priv_rsa, gy + gx)
    conn.send(mkpair(mkpair(gy, sigB), certB_bytes))

    # 3. Alice → Bob: SigA(g^x || g^y) || CertA
    msg = conn.recv()
    sigA, certA_bytes = unpair(msg)

    # Verificar CertA contra CA
    ca_cert = load_cert("CA.crt")
    certA   = load_pem_x509_certificate(certA_bytes)
    try:
        verify_cert(certA, ca_cert)
        print("[Bob]   CertA verificado pela CA ✓")
    except Exception as e:
        print(f"[Bob]   ERRO: CertA inválido: {e}")
        return

    # Verificar assinatura da Alice: SigA(g^x || g^y)
    try:
        verify_sig(certA.public_key(), sigA, gx + gy)
        print("[Bob]   Assinatura da Alice verificada ✓")
    except Exception as e:
        print(f"[Bob]   ERRO: assinatura Alice inválida: {e}")
        return

    # 4. Calcular K e derivar chave
    alice_pub_dh = serialization.load_pem_public_key(gx)
    key          = derive_key(bob_priv_dh.exchange(alice_pub_dh))
    print(f"[Bob]   chave AES-GCM: {key.hex()}")

    # 5. Receber e decifrar mensagem da Alice
    data = conn.recv()
    msg  = AESGCM(key).decrypt(data[:NONCE_SIZE], data[NONCE_SIZE:], None)
    print(f"[Bob]   recebeu: {msg.decode()}")

    # 6. Responder cifrado
    reply  = b"Ola Alice! Bob confirmou autenticidade. STS completo."
    nonce  = os.urandom(NONCE_SIZE)
    ct     = AESGCM(key).encrypt(nonce, reply, None)
    conn.send(nonce + ct)

if __name__ == '__main__':
    parent_conn, child_conn = Pipe()
    p1 = Process(target=alice_process, args=(parent_conn,))
    p2 = Process(target=bob_process,   args=(child_conn,))
    p1.start(); p2.start()
    p1.join();  p2.join()
