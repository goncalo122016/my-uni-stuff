import os
from datetime import datetime, timedelta

from cryptography.exceptions import InvalidSignature, InvalidTag
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ed25519, x25519
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.hkdf import HKDF
from cryptography import x509
from cryptography.x509.oid import NameOID

# ==========================================
# GESTÃO DE IDENTIDADE E ASSINATURAS (Ed25519)
# ==========================================
# Justificação: Ed25519 é o standard moderno para assinaturas digitais.
# É rápido, seguro contra ataques de side-channel e gera chaves/assinaturas pequenas.
# Resolve a ameaça de Spoofing de Identidade (garante Autenticidade e Integridade da origem).


def generate_ed25519_keypair():
    """Gera um par de chaves Ed25519 para identidade e assinaturas."""
    private_key = ed25519.Ed25519PrivateKey.generate()
    public_key = private_key.public_key()
    return private_key, public_key


def sign_message(private_key, message: bytes) -> bytes:
    """Assina uma mensagem (bytes) usando a chave privada Ed25519."""
    return private_key.sign(message)


def verify_signature(public_key, message: bytes, signature: bytes) -> bool:
    """Verifica a assinatura de uma mensagem usando a chave pública Ed25519."""
    try:
        public_key.verify(signature, message)
        return True
    except InvalidSignature:
        return False


# ==========================================
# ACORDO DE CHAVES (X25519 - ECDH)
# ==========================================
# Justificação: X25519 é otimizado para a troca de chaves Diffie-Hellman em curvas elíticas (ECDH).
# Permite que duas partes estabeleçam um segredo partilhado de forma segura sobre um canal inseguro.
# Base para a confidencialidade no canal Cliente-Servidor e no canal E2EE Cliente-Cliente.


def generate_x25519_keypair():
    """Gera um par de chaves efémeras ou estáticas X25519 para troca de chaves."""
    private_key = x25519.X25519PrivateKey.generate()
    public_key = private_key.public_key()
    return private_key, public_key


def compute_shared_secret(private_key, peer_public_key) -> bytes:
    """Calcula o segredo partilhado usando a chave privada local e a pública do destinatário."""
    return private_key.exchange(peer_public_key)


# ==========================================
# DERIVAÇÃO DE CHAVES (HKDF)
# ==========================================
# Justificação: O resultado de um Diffie-Hellman (X25519) não é uma chave uniformemente aleatória.
# O HKDF (com SHA-256) extrai a entropia do segredo partilhado e expande-a para uma chave
# de tamanho adequado (ex: 32 bytes para AES-256), tornando-a criptograficamente forte.


def derive_key(
    shared_secret: bytes,
    salt: bytes = None,
    info: bytes = b"chat_e2ee",
    length: int = 32,
) -> bytes:
    """Deriva uma chave simétrica a partir de um segredo partilhado usando HKDF-SHA256."""
    hkdf = HKDF(
        algorithm=hashes.SHA256(),
        length=length,
        salt=salt,
        info=info,
    )
    return hkdf.derive(shared_secret)


# ==========================================
# CIFRAGEM AUTENTICADA (AES-GCM)
# ==========================================
# Justificação: AES-GCM (Galois/Counter Mode) fornece Cifragem Autenticada com Dados Associados (AEAD).
# Garante simultaneamente a Confidencialidade e a Integridade dos dados. Se a mensagem for
# alterada em trânsito (MITM), a tag de autenticação falha e a decifragem é rejeitada.


def generate_nonce(size: int = 12) -> bytes:
    """Gera um Nonce (Number Used Once) criptograficamente seguro. O standard para GCM é 12 bytes."""
    return os.urandom(size)


def encrypt_aes_gcm(
    key: bytes, plaintext: bytes, associated_data: bytes = None
) -> tuple[bytes, bytes]:
    """
    Cifra dados usando AES-GCM.
    Retorna (nonce, ciphertext) onde o ciphertext inclui a tag de autenticação no final.
    """
    aesgcm = AESGCM(key)
    nonce = generate_nonce()
    # A biblioteca cryptography anexa a tag de 16 bytes automaticamente ao final do ciphertext
    ciphertext = aesgcm.encrypt(nonce, plaintext, associated_data)
    return nonce, ciphertext


def decrypt_aes_gcm(
    key: bytes, nonce: bytes, ciphertext: bytes, associated_data: bytes = None
) -> bytes:
    """
    Decifra dados usando AES-GCM e verifica a integridade/autenticidade.
    Lança ValueError se a decifragem falhar (ex: tag inválida, dados corrompidos ou chave errada).
    """
    aesgcm = AESGCM(key)
    try:
        plaintext = aesgcm.decrypt(nonce, ciphertext, associated_data)
        return plaintext
    except InvalidTag:
        raise ValueError(
            "Falha na decifragem: Integridade ou Autenticidade comprometida (Tag GCM inválida)."
        )


# ==========================================
# SERIALIZAÇÃO DE CHAVES (ARMAZENAMENTO E REDE)
# ==========================================
# Justificação: As chaves em memória precisam de ser convertidas para bytes (Raw ou PEM)
# para serem guardadas no disco (persistencia) ou enviadas pelo socket (rede).


def private_key_to_pem(private_key, password: bytes = None) -> bytes:
    """Serializa uma chave privada para o formato PEM. Pode ser encriptada com password (opcional)."""
    encryption_algo = (
        serialization.BestAvailableEncryption(password)
        if password
        else serialization.NoEncryption()
    )
    return private_key.private_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PrivateFormat.PKCS8,
        encryption_algorithm=encryption_algo,
    )


def public_key_to_pem(public_key) -> bytes:
    """Serializa uma chave pública para o formato PEM (útil para guardar em ficheiro)."""
    return public_key.public_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PublicFormat.SubjectPublicKeyInfo,
    )


def public_key_to_raw(public_key) -> bytes:
    """Serializa uma chave pública para formato Raw (apenas os bytes da chave, ideal para enviar na rede)."""
    return public_key.public_bytes(
        encoding=serialization.Encoding.Raw, format=serialization.PublicFormat.Raw
    )


def load_private_key_from_pem(pem_bytes: bytes, password: bytes = None):
    """Carrega uma chave privada a partir de bytes PEM."""
    return serialization.load_pem_private_key(pem_bytes, password=password)


def load_public_key_from_pem(pem_bytes: bytes):
    """Carrega uma chave pública a partir de bytes PEM."""
    return serialization.load_pem_public_key(pem_bytes)


def load_ed25519_public_key_from_raw(raw_bytes: bytes):
    """Reconstrói uma chave pública Ed25519 a partir dos bytes Raw recebidos da rede."""
    return ed25519.Ed25519PublicKey.from_public_bytes(raw_bytes)


def load_x25519_public_key_from_raw(raw_bytes: bytes):
    """Reconstrói uma chave pública X25519 a partir dos bytes Raw recebidos da rede."""
    return x25519.X25519PublicKey.from_public_bytes(raw_bytes)


# ==========================================
# GESTÃO DE CERTIFICADOS X.509 (CA)
# ==========================================
# Justificação: O servidor atua como Autoridade Certificadora (CA).
# Emite certificados X.509 auto-assinados para si próprio e certificados assinados para os clientes.
# Isto garante autenticação e permite que os clientes validem a identidade uns dos outros.


def generate_self_signed_ca_certificate(
    private_key, common_name: str = "Server-CA", days_valid: int = 3650
) -> bytes:
    """
    Gera um certificado X.509 auto-assinado para a Autoridade Certificadora.
    Retorna o certificado em formato PEM.
    """
    subject = issuer = x509.Name([
        x509.NameAttribute(NameOID.COMMON_NAME, common_name),
        x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Chat E2EE"),
    ])

    cert = x509.CertificateBuilder().subject_name(
        subject
    ).issuer_name(
        issuer
    ).public_key(
        private_key.public_key()
    ).serial_number(
        x509.random_serial_number()
    ).not_valid_before(
        datetime.utcnow()
    ).not_valid_after(
        datetime.utcnow() + timedelta(days=days_valid)
    ).add_extension(
        x509.BasicConstraints(ca=True, path_length=None),
        critical=True,
    ).add_extension(
        x509.KeyUsage(
            digital_signature=True,
            key_cert_sign=True,
            crl_sign=True,
            key_encipherment=False,
            content_commitment=False,
            data_encipherment=False,
            key_agreement=False,
            encipher_only=False,
            decipher_only=False,
        ),
        critical=True,
    ).sign(
        private_key, None
    )

    return cert.public_bytes(serialization.Encoding.PEM)


def generate_client_certificate(
    ca_private_key,
    client_public_key,
    username: str,
    days_valid: int = 365,
) -> bytes:
    """
    Gera um certificado X.509 para um cliente, assinado pela CA.
    Retorna o certificado em formato PEM.
    """
    subject = x509.Name([
        x509.NameAttribute(NameOID.COMMON_NAME, username),
        x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Chat E2EE"),
    ])

    issuer = x509.Name([
        x509.NameAttribute(NameOID.COMMON_NAME, "Server-CA"),
        x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Chat E2EE"),
    ])

    cert = x509.CertificateBuilder().subject_name(
        subject
    ).issuer_name(
        issuer
    ).public_key(
        client_public_key
    ).serial_number(
        x509.random_serial_number()
    ).not_valid_before(
        datetime.utcnow()
    ).not_valid_after(
        datetime.utcnow() + timedelta(days=days_valid)
    ).add_extension(
        x509.BasicConstraints(ca=False, path_length=None),
        critical=True,
    ).add_extension(
        x509.KeyUsage(
            digital_signature=True,
            key_cert_sign=False,
            crl_sign=False,
            key_encipherment=False,
            content_commitment=False,
            data_encipherment=False,
            key_agreement=False,
            encipher_only=False,
            decipher_only=False,
        ),
        critical=True,
    ).sign(
        ca_private_key, None
    )

    return cert.public_bytes(serialization.Encoding.PEM)


def load_certificate_from_pem(pem_bytes: bytes):
    """Carrega um certificado X.509 a partir de bytes PEM."""
    return x509.load_pem_x509_certificate(pem_bytes)


def verify_certificate_chain(cert_pem: bytes, ca_cert_pem: bytes) -> bool:
    """
    Verifica se um certificado foi assinado pela CA.
    Retorna True se o certificado é válido e foi assinado pela CA, False caso contrário.
    """
    try:
        cert = load_certificate_from_pem(cert_pem)
        ca_cert = load_certificate_from_pem(ca_cert_pem)
        ca_public_key = ca_cert.public_key()

        # Verificar a assinatura do certificado usando a chave pública da CA
        # Para Ed25519, não passamos o hash algorithm
        ca_public_key.verify(cert.signature, cert.tbs_certificate_bytes)
        
        # Verificar se o certificado foi emitido pela CA verificando o issuer
        issuer_cn = cert.issuer.get_attributes_for_oid(NameOID.COMMON_NAME)
        ca_subject_cn = ca_cert.subject.get_attributes_for_oid(NameOID.COMMON_NAME)
        
        if issuer_cn and ca_subject_cn and issuer_cn[0].value == ca_subject_cn[0].value:
            return True
        
        return False
    except Exception:
        return False


def extract_username_from_certificate(cert_pem: bytes) -> str:
    """Extrai o Common Name (username) do certificado."""
    try:
        cert = load_certificate_from_pem(cert_pem)
        cn = cert.subject.get_attributes_for_oid(NameOID.COMMON_NAME)
        if cn:
            return cn[0].value
    except Exception:
        pass
    return None
