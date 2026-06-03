import sys
import os
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.padding import PKCS7
from cryptography.hazmat.backends import default_backend

KEY_SIZE = 32
IV_SIZE = 16

def setup(fkey):
    key = os.urandom(KEY_SIZE)
    with open(fkey, "wb") as f:
        f.write(key)

def enc(fich, fkey):

    key = open(fkey,"rb").read()
    plaintext = open(fich,"rb").read()

    iv = os.urandom(IV_SIZE)

    padder = PKCS7(128).padder()
    padded = padder.update(plaintext) + padder.finalize()

    cipher = Cipher(algorithms.AES(key), modes.CBC(iv), backend=default_backend())
    encryptor = cipher.encryptor()

    ciphertext = encryptor.update(padded) + encryptor.finalize()

    with open(fich+".enc","wb") as f:
        f.write(iv + ciphertext)

def dec(fich, fkey):

    key = open(fkey,"rb").read()
    data = open(fich,"rb").read()

    iv = data[:16]
    ciphertext = data[16:]

    cipher = Cipher(algorithms.AES(key), modes.CBC(iv), backend=default_backend())
    decryptor = cipher.decryptor()

    padded = decryptor.update(ciphertext) + decryptor.finalize()

    unpadder = PKCS7(128).unpadder()
    plaintext = unpadder.update(padded) + unpadder.finalize()

    with open(fich+".dec","wb") as f:
        f.write(plaintext)

op = sys.argv[1]

if op=="setup":
    setup(sys.argv[2])
elif op=="enc":
    enc(sys.argv[2],sys.argv[3])
elif op=="dec":
    dec(sys.argv[2],sys.argv[3])