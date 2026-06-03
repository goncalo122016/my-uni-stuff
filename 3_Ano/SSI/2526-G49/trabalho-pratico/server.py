import argparse
import socket
import threading
from typing import Any, Dict
import os

import utils.crypto_utils as cu
from protocol import protocol as prot
from data import storage

# ==========================================
# SERVIDOR (HONESTO MAS CURIOSO)
# Justificação: O servidor gere os sockets, faz o handshake inicial (autenticação mútua)
# e encaminha mensagens E2EE através do módulo storage.py. Não tem acesso ao plaintext.

HOST = "127.0.0.1"
PORT = 8443

# Chaves estáticas do Servidor (Carregadas ou geradas na inicialização)
SERVER_PRIV_ED = None
SERVER_PUB_ED = None

# Certificado da CA do Servidor
CA_CERT_PEM = None

# Cache temporária de clientes conectados
# {username: SecureChannel}
_connected_clients: Dict[str, prot.SecureChannel] = {}
_client_lock = threading.Lock()


def load_server_keys():
    """Carrega ou cria as chaves do servidor e o certificado da CA."""
    global SERVER_PRIV_ED, SERVER_PUB_ED, CA_CERT_PEM

    key_dir = "keys/server"
    if not os.path.exists(key_dir):
        os.makedirs(key_dir)

    priv_path = os.path.join(key_dir, "server_ed25519.pem")
    ca_cert_path = os.path.join(key_dir, "ca_cert.pem")

    if os.path.exists(priv_path):
        with open(priv_path, "rb") as f:
            SERVER_PRIV_ED = cu.load_private_key_from_pem(f.read())
        SERVER_PUB_ED = SERVER_PRIV_ED.public_key()
        print("[+] Chaves do servidor carregadas.")
    else:
        SERVER_PRIV_ED, SERVER_PUB_ED = cu.generate_ed25519_keypair()
        with open(priv_path, "wb") as f:
            f.write(cu.private_key_to_pem(SERVER_PRIV_ED))
        print("[+] Novas chaves do servidor geradas.")

    # Gerar ou carregar o certificado da CA
    if os.path.exists(ca_cert_path):
        with open(ca_cert_path, "rb") as f:
            CA_CERT_PEM = f.read()
        print("[+] Certificado da CA carregado.")
    else:
        CA_CERT_PEM = cu.generate_self_signed_ca_certificate(
            SERVER_PRIV_ED,
            common_name="Server-CA",
            days_valid=3650
        )
        with open(ca_cert_path, "wb") as f:
            f.write(CA_CERT_PEM)
        print("[+] Novo certificado da CA gerado.")

    # Expõe a pública do servidor para os clientes (simplificação académica)
    pub_path = os.path.join("keys", "server_pub.pem")
    with open(pub_path, "wb") as f:
        f.write(cu.public_key_to_pem(SERVER_PUB_ED))

    # Expõe o certificado da CA para os clientes
    ca_pub_path = os.path.join("keys", "ca_cert.pem")
    with open(ca_pub_path, "wb") as f:
        f.write(CA_CERT_PEM)


def handle_client(conn: socket.socket, addr):
    print(f"[+] Conexão de {addr}")
    username = None
    try:
        # 1. IDENTIFICAÇÃO DO CLIENTE (Não segura, mas base para o Handshake)
        # O cliente envia o seu nome e nós carregamos a chave pública Ed25519 dele
        # para conseguirmos fazer a verificação de assinatura.
        init_msg = prot.recv_raw_msg(conn)
        username = init_msg.get("username")
        client_pub_ed_pem_path = f"keys/{username}_pub.pem"

        if not os.path.exists(client_pub_ed_pem_path):
            print(f"[-] Rejeitado: Chave pública não encontrada para {username}.")
            conn.close()
            return

        with open(client_pub_ed_pem_path, "rb") as f:
            peer_pub_ed = cu.load_public_key_from_pem(f.read())

        # 2. HANDSHAKE SEGURO
        # O servidor estabelece o canal TLS-like garantindo Forward Secrecy parcial
        secure_channel = prot.server_handshake(conn, SERVER_PRIV_ED, peer_pub_ed)
        print(f"[+] Canal seguro estabelecido com {username}.")

        with _client_lock:
            _connected_clients[username] = secure_channel

        # Enviar o certificado da CA ao cliente
        secure_channel.send(
            {"action": "CA_CERT", "ca_cert": prot.encode_base64(CA_CERT_PEM)}
        )

        # 3. LOOP DE COMANDOS SEGURO
        while True:
            try:
                msg = secure_channel.recv()
            except ValueError as e:
                print(f"[-] Adulteração detectada em {username}: {e}")
                break
            except Exception:
                break

            action = msg.get("action")
            if not action:
                continue

            # REGISTER
            if action == "REGISTER":
                # Cliente diz as suas chaves X25519 públicas para E2EE
                ed_pub_b64 = msg.get("ed25519_pub")
                x_pub_b64 = msg.get("x25519_pub")

                # Registar o utilizador
                storage.register_user(username, ed_pub_b64, x_pub_b64)

                # Gerar um certificado para o cliente
                try:
                    client_pub_ed_raw = prot.decode_base64(ed_pub_b64)
                    client_pub_ed = cu.load_ed25519_public_key_from_raw(client_pub_ed_raw)

                    # Gerar certificado assinado pela CA
                    client_cert_pem = cu.generate_client_certificate(
                        SERVER_PRIV_ED,
                        client_pub_ed,
                        username,
                        days_valid=365
                    )

                    # Armazenar o certificado do cliente
                    cert_dir = "keys/clients"
                    if not os.path.exists(cert_dir):
                        os.makedirs(cert_dir)

                    cert_path = os.path.join(cert_dir, f"{username}_cert.pem")
                    with open(cert_path, "wb") as f:
                        f.write(client_cert_pem)

                    # Enviar o certificado ao cliente
                    secure_channel.send(
                        {
                            "status": "OK",
                            "msg": "Registado com sucesso no servidor.",
                            "certificate": prot.encode_base64(client_cert_pem)
                        }
                    )
                    print(f"[+] Certificado gerado para {username}.")
                except Exception as e:
                    print(f"[-] Erro ao gerar certificado para {username}: {e}")
                    secure_channel.send(
                        {"status": "ERROR", "msg": "Erro ao gerar certificado."}
                    )

            # LIST_USERS
            elif action == "LIST_USERS":
                users = storage.list_users()
                secure_channel.send({"status": "OK", "users": users})

            # GET_KEY
            elif action == "GET_KEY":
                target = msg.get("target")
                keys = storage.get_user_keys(target)
                if keys:
                    # Incluir o certificado X.509 do utilizador na resposta para
                    # permitir que o cliente valide a identidade do destinatário via CA.
                    cert_path = os.path.join("keys", "clients", f"{target}_cert.pem")
                    target_cert_b64 = None
                    if os.path.exists(cert_path):
                        with open(cert_path, "rb") as f:
                            target_cert_b64 = prot.encode_base64(f.read())

                    secure_channel.send(
                        {
                            "status": "OK",
                            "target": target,
                            "keys": keys,
                            "certificate": target_cert_b64,
                        }
                    )
                else:
                    secure_channel.send(
                        {"status": "ERROR", "msg": "Utilizador não encontrado."}
                    )

            # SEND_MSG (E2EE)
            elif action == "SEND_MSG":
                target = msg.get("target")
                timestamp = msg.get("timestamp")
                e2e_nonce = msg.get("e2e_nonce")
                e2e_ciphertext = msg.get("e2e_ciphertext")

                # Verificar se o destinatário existe antes de qualquer coisa
                if not storage.get_user_keys(target):
                    secure_channel.send(
                        {"status": "ERROR", "msg": "Destinatário inexistente."}
                    )
                    continue

                # Guardar sempre no storage para histórico persistente e para
                # garantir entrega caso o cliente destino caia entre o push e a receção.
                storage.store_message(
                    target, username, timestamp, e2e_nonce, e2e_ciphertext
                )

                # Confirmar ao remetente imediatamente
                secure_channel.send(
                    {
                        "status": "OK",
                        "msg": f"Mensagem enviada para {target}.",
                    }
                )

                # Se o target estiver online, entregar em tempo real.
                # O servidor continua a ver apenas bytes opacos (E2EE mantido).
                with _client_lock:
                    target_channel = _connected_clients.get(target)

                if target_channel is not None:
                    try:
                        target_channel.send(
                            {
                                "action": "DELIVER_MSG",
                                "sender": username,
                                "timestamp": timestamp,
                                "e2e_nonce": e2e_nonce,
                                "e2e_ciphertext": e2e_ciphertext,
                            }
                        )
                        print(f"[+] Mensagem de {username} entregue em tempo real a {target}.")
                    except Exception as e:
                        # O cliente pode ter desligado entre o lookup e o send.
                        # A mensagem já está no storage, por isso não há perda.
                        print(f"[-] Falha na entrega em tempo real a {target}: {e}. Ficará pendente no histórico.")

            # DELETE_USER
            elif action == "DELETE_USER":
                if storage.delete_user(username):
                    secure_channel.send({"status": "OK", "msg": "Conta eliminada com sucesso."})
                    break  # Fechar conexão
                else:
                    secure_channel.send({"status": "ERROR", "msg": "Erro ao eliminar conta."})

            # FETCH_MSGS
            elif action == "FETCH_MSGS":
                # Cliente pede as mensagens pendentes (assíncrono / offline)
                msgs = storage.get_and_clear_messages(username)
                secure_channel.send({"status": "OK", "messages": msgs})

            else:
                secure_channel.send({"status": "ERROR", "msg": "Comando desconhecido."})

    except Exception as e:
        print(f"[-] Erro com {username if username else addr}: {e}")
    finally:
        print(f"[-] Conexão fechada com {username if username else addr}")
        with _client_lock:
            if username and username in _connected_clients:
                del _connected_clients[username]
        conn.close()


def start_server():
    storage.init_storage()
    load_server_keys()

    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind((HOST, PORT))
    server.listen(5)

    print(f"[*] Servidor a escutar em {HOST}:{PORT}")

    try:
        while True:
            conn, addr = server.accept()
            threading.Thread(
                target=handle_client, args=(conn, addr), daemon=True
            ).start()
    except KeyboardInterrupt:
        print("\n[*] Encerrando servidor.")
        server.close()


if __name__ == "__main__":
    start_server()