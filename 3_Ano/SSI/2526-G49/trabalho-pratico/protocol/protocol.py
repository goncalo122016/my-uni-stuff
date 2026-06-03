import base64
import json
import socket
import struct
from typing import Any, Dict

import utils.crypto_utils as cu


def encode_base64(data: bytes) -> str:
    """Codifica bytes para string Base64."""
    return base64.b64encode(data).decode("utf-8")


def decode_base64(data: str) -> bytes:
    """Descodifica string Base64 para bytes."""
    return base64.b64decode(data.encode("utf-8"))


def send_raw_msg(sock: socket.socket, msg_dict: Dict[str, Any]):
    """
    Serializa um dicionário para JSON, codifica em UTF-8 e envia pelo socket.
    O formato no socket é: [Tamanho da mensagem (4 bytes struct)][Mensagem JSON]
    """
    json_data = json.dumps(msg_dict).encode("utf-8")
    # Empacota o tamanho da mensagem num inteiro de 4 bytes (network byte order)
    msg_len = struct.pack("!I", len(json_data))
    sock.sendall(msg_len + json_data)


def recv_raw_msg(sock: socket.socket) -> Dict[str, Any]:
    """
    Lê o tamanho da mensagem (4 bytes) e depois o JSON completo do socket.
    Deserializa para dicionário.
    """
    # Ler o cabeçalho de 4 bytes
    raw_msglen = _recv_all(sock, 4)
    if not raw_msglen:
        raise ConnectionError("Conexão fechada pelo parceiro.")
    msglen = struct.unpack("!I", raw_msglen)[0]

    # Ler os dados da mensagem baseados no tamanho recebido
    json_data = _recv_all(sock, msglen)
    if not json_data:
        raise ConnectionError("Falha ao ler os dados completos da mensagem.")

    return json.loads(json_data.decode("utf-8"))


def _recv_all(sock: socket.socket, n: int) -> bytes:
    """Função auxiliar para garantir que se leem exatemente n bytes do socket TCP."""
    data = bytearray()
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            return None
        data.extend(packet)
    return bytes(data)


class SecureChannel:
    """
    Abstrai um socket TCP garantindo Confidencialidade, Integridade e Autenticidade
    através de uma chave de sessão simétrica (AES-GCM).
    """

    def __init__(self, sock: socket.socket, session_key: bytes):
        self.sock = sock
        self.session_key = session_key

    def send(self, payload_dict: Dict[str, Any]):
        """Cifra o payload JSON com AES-GCM e envia o envelope cifrado."""
        plaintext = json.dumps(payload_dict).encode("utf-8")
        nonce, ciphertext = cu.encrypt_aes_gcm(self.session_key, plaintext)

        envelope = {
            "nonce": encode_base64(nonce),
            "ciphertext": encode_base64(ciphertext),
        }
        send_raw_msg(self.sock, envelope)

    def recv(self) -> Dict[str, Any]:
        """Recebe o envelope cifrado, decifra com AES-GCM e retorna o payload original."""
        envelope = recv_raw_msg(self.sock)

        if "nonce" not in envelope or "ciphertext" not in envelope:
            raise ValueError("Envelope inválido ou não cifrado.")

        nonce = decode_base64(envelope["nonce"])
        ciphertext = decode_base64(envelope["ciphertext"])

        plaintext = cu.decrypt_aes_gcm(self.session_key, nonce, ciphertext)
        return json.loads(plaintext.decode("utf-8"))


def client_handshake(sock: socket.socket, my_priv_ed, peer_pub_ed) -> SecureChannel:
    """
    Executa o lado do Cliente do Handshake (Acordo de Chave Autenticado).
    1. Gera chave efémera X25519.
    2. Envia chave pública efémera assinada pela sua Ed25519 estática.
    3. Recebe chave pública efémera do Servidor e valida a assinatura.
    4. Deriva a chave de sessão.
    """
    # 1. Gerar chaves efémeras
    priv_x, pub_x = cu.generate_x25519_keypair()
    raw_pub_x = cu.public_key_to_raw(pub_x)

    # 2. Assinar e enviar
    signature = cu.sign_message(my_priv_ed, raw_pub_x)
    send_raw_msg(
        sock,
        {
            "ephemeral_pub": encode_base64(raw_pub_x),
            "signature": encode_base64(signature),
        },
    )

    # 3. Receber e verificar o Servidor
    server_msg = recv_raw_msg(sock)
    srv_raw_pub_x = decode_base64(server_msg["ephemeral_pub"])
    srv_sig = decode_base64(server_msg["signature"])

    if not cu.verify_signature(peer_pub_ed, srv_raw_pub_x, srv_sig):
        raise ValueError("Handshake falhou: Assinatura do Servidor inválida.")

    srv_pub_x = cu.load_x25519_public_key_from_raw(srv_raw_pub_x)

    # 4. Derivar chave de sessão
    shared_secret = cu.compute_shared_secret(priv_x, srv_pub_x)
    session_key = cu.derive_key(shared_secret, info=b"tcp_transport_kdf")

    return SecureChannel(sock, session_key)


def server_handshake(sock: socket.socket, my_priv_ed, peer_pub_ed) -> SecureChannel:
    """
    Executa o lado do Servidor do Handshake.
    Complementar ao client_handshake.
    """
    # 1. Receber do Cliente e verificar
    client_msg = recv_raw_msg(sock)
    cli_raw_pub_x = decode_base64(client_msg["ephemeral_pub"])
    cli_sig = decode_base64(client_msg["signature"])

    if not cu.verify_signature(peer_pub_ed, cli_raw_pub_x, cli_sig):
        raise ValueError("Handshake falhou: Assinatura do Cliente inválida.")

    cli_pub_x = cu.load_x25519_public_key_from_raw(cli_raw_pub_x)

    # 2. Gerar chaves efémeras
    priv_x, pub_x = cu.generate_x25519_keypair()
    raw_pub_x = cu.public_key_to_raw(pub_x)

    # 3. Assinar e enviar
    signature = cu.sign_message(my_priv_ed, raw_pub_x)
    send_raw_msg(
        sock,
        {
            "ephemeral_pub": encode_base64(raw_pub_x),
            "signature": encode_base64(signature),
        },
    )

    # 4. Derivar chave de sessão
    shared_secret = cu.compute_shared_secret(priv_x, cli_pub_x)
    session_key = cu.derive_key(shared_secret, info=b"tcp_transport_kdf")

    return SecureChannel(sock, session_key)
