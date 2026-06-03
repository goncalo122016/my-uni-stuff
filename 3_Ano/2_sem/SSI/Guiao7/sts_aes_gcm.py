from multiprocessing import Process, Pipe
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import dh, padding
from cryptography.hazmat.primitives.kdf.hkdf import HKDF
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography import x509
import os

p = int("""
FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E08
8A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B
302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9
A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE6
49286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8
FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D
670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C
180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF695581718
3995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF
""".replace("\n", ""), 16)

g = 2
parameters = dh.DHParameterNumbers(p, g).parameters()

NONCE_SIZE = 12

def mkpair(x, y):
    return len(x).to_bytes(2, "little") + x + y

def unpair(xy):
    l = int.from_bytes(xy[:2], "little")
    return xy[2:2+l], xy[2+l:]

def load_cert(path):
    with open(path, "rb") as f:
        return x509.load_pem_x509_certificate(f.read())

def load_privkey(path):
    with open(path, "rb") as f:
        return serialization.load_pem_private_key(f.read(), password=None)

def alice_process(conn):
    priv = parameters.generate_private_key()
    pub = priv.public_key()
    gx = pub.public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo
    )

    # enviar gx
    conn.send(gx)

    # receber gy, sigB, certB
    msg = conn.recv()
    part1, certB_bytes = unpair(msg)
    gy_bytes, sigB = unpair(part1)

    certB = x509.load_pem_x509_certificate(certB_bytes)
    bob_rsa_pub = certB.public_key()

    bob_rsa_pub.verify(
        sigB,
        gy_bytes + gx,
        padding.PSS(
            mgf=padding.MGF1(hashes.SHA256()),
            salt_length=padding.PSS.MAX_LENGTH
        ),
        hashes.SHA256()
    )

    bob_pub = serialization.load_pem_public_key(gy_bytes)
    shared = priv.exchange(bob_pub)

    key = HKDF(
        algorithm=hashes.SHA256(),
        length=32,
        salt=None,
        info=b"sts"
    ).derive(shared)

    # carregar chave RSA + cert
    alice_priv = load_privkey("Alice.key")
    with open("Alice.crt", "rb") as f:
        certA_bytes = f.read()

    # assinar gx || gy
    sigA = alice_priv.sign(
        gx + gy_bytes,
        padding.PSS(
            mgf=padding.MGF1(hashes.SHA256()),
            salt_length=padding.PSS.MAX_LENGTH
        ),
        hashes.SHA256()
    )

    conn.send(mkpair(sigA, certA_bytes))

    aesgcm = AESGCM(key)
    nonce = os.urandom(NONCE_SIZE)
    ct = aesgcm.encrypt(nonce, b"Hello Bob (STS)!", None)

    conn.send(nonce + ct)

def bob_process(conn):
    priv = parameters.generate_private_key()
    pub = priv.public_key()
    gy = pub.public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo
    )

    # receber gx
    gx = conn.recv()

    # carregar chave RSA + cert
    bob_priv = load_privkey("Bob.key")
    with open("Bob.crt", "rb") as f:
        certB_bytes = f.read()

    # assinar gy || gx
    sigB = bob_priv.sign(
        gy + gx,
        padding.PSS(
            mgf=padding.MGF1(hashes.SHA256()),
            salt_length=padding.PSS.MAX_LENGTH
        ),
        hashes.SHA256()
    )

    conn.send(mkpair(mkpair(gy, sigB), certB_bytes))

    # receber assinatura Alice
    msg = conn.recv()
    sigA, certA_bytes = unpair(msg)

    certA = x509.load_pem_x509_certificate(certA_bytes)
    alice_rsa_pub = certA.public_key()

    alice_rsa_pub.verify(
        sigA,
        gx + gy,
        padding.PSS(
            mgf=padding.MGF1(hashes.SHA256()),
            salt_length=padding.PSS.MAX_LENGTH
        ),
        hashes.SHA256()
    )

    alice_pub = serialization.load_pem_public_key(gx)
    shared = priv.exchange(alice_pub)

    key = HKDF(
        algorithm=hashes.SHA256(),
        length=32,
        salt=None,
        info=b"sts"
    ).derive(shared)

    data = conn.recv()
    nonce = data[:NONCE_SIZE]
    ct = data[NONCE_SIZE:]

    aesgcm = AESGCM(key)
    pt = aesgcm.decrypt(nonce, ct, None)

    print("Bob recebeu:", pt.decode())

if __name__ == "__main__":
    parent, child = Pipe()
    a = Process(target=alice_process, args=(parent,))
    b = Process(target=bob_process, args=(child,))
    a.start(); b.start()
    a.join(); b.join()