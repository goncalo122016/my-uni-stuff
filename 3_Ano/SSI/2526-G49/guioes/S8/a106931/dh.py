from multiprocessing import Process, Pipe
from cryptography.hazmat.primitives.asymmetric import dh
from cryptography.hazmat.primitives import serialization, hashes
from cryptography.hazmat.primitives.kdf.hkdf import HKDF

# Parâmetros DH públicos (RFC 3526 grupo 2, 1024 bits)
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

def alice_process(conn):
    # Gerar par de chaves DH efémero
    alice_priv = parameters.generate_private_key()
    alice_pub  = alice_priv.public_key()

    # 1. Alice → Bob: envia g^x serializado
    gx = alice_pub.public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo
    )
    conn.send(gx)

    # 2. Bob → Alice: recebe g^y
    gy_bytes = conn.recv()

    # 3. Calcular segredo partilhado K = g^(xy)
    bob_pub = serialization.load_pem_public_key(gy_bytes)
    shared  = alice_priv.exchange(bob_pub)

    # Derivar chave simétrica de K via HKDF
    key = HKDF(
        algorithm=hashes.SHA256(), length=32,
        salt=None, info=b'dh-handshake'
    ).derive(shared)

    print(f"[Alice] K derivado: {key.hex()}")

def bob_process(conn):
    # Gerar par de chaves DH efémero
    bob_priv = parameters.generate_private_key()
    bob_pub  = bob_priv.public_key()

    # 1. Alice → Bob: recebe g^x
    gx_bytes = conn.recv()

    # 2. Bob → Alice: envia g^y serializado
    gy = bob_pub.public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo
    )
    conn.send(gy)

    # 3. Calcular segredo partilhado K = g^(xy)
    alice_pub = serialization.load_pem_public_key(gx_bytes)
    shared    = bob_priv.exchange(alice_pub)

    key = HKDF(
        algorithm=hashes.SHA256(), length=32,
        salt=None, info=b'dh-handshake'
    ).derive(shared)

    print(f"[Bob]   K derivado: {key.hex()}")

if __name__ == '__main__':
    parent_conn, child_conn = Pipe()
    p1 = Process(target=alice_process, args=(parent_conn,))
    p2 = Process(target=bob_process,   args=(child_conn,))
    p1.start(); p2.start()
    p1.join();  p2.join()
