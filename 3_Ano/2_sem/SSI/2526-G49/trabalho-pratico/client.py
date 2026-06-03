import datetime
import getpass
import json
import os
import queue
import socket
import sys
import threading
from typing import Dict, Optional

from argon2 import PasswordHasher
from argon2.exceptions import VerifyMismatchError, VerificationError, InvalidHashError

import utils.crypto_utils as cu
from protocol import protocol as prot

HOST = "127.0.0.1"
PORT = 8443

# Argon2 hasher — parâmetros recomendados para segurança interativa (OWASP 2024)
# time_cost=3, memory_cost=65536 (64 MiB), parallelism=4
_ph = PasswordHasher(time_cost=3, memory_cost=65536, parallelism=4)

# Caminho único para o ficheiro de utilizadores
USERS_JSON = os.path.join("keys", "users.json")

# Chaves estáticas do Cliente (Identidade e E2EE) — repostas a None no logout
CLIENT_PRIV_ED = None
CLIENT_PUB_ED = None
CLIENT_PRIV_X = None
CLIENT_PUB_X = None

SERVER_PUB_ED = None

# Cache de chaves E2E derivadas: {username: bytes}
_e2e_key_cache: Dict[str, bytes] = {}
_key_cache_lock = threading.Lock()

# Locks dedicados a I/O de ficheiros partilhados entre threads
_users_json_lock = threading.Lock()  # protege users.json (login e session em paralelo)
_history_lock = threading.Lock()     # protege history JSON (listener vs thread principal)

# Fila de respostas síncronas (LIST, FETCH, SEND confirmação, etc.)
_response_queue: queue.Queue = queue.Queue()

# Username global (lido pelo listener thread para o prompt)
_username: str = ""


# ──────────────────────────────────────────────────────────────
# GESTÃO DO FICHEIRO users.json
# ──────────────────────────────────────────────────────────────

def _load_users() -> dict:
    """Carrega o ficheiro users.json. Devolve estrutura vazia se não existir."""
    if not os.path.exists(USERS_JSON):
        return {"users": {}}
    with open(USERS_JSON, "r", encoding="utf-8") as f:
        try:
            return json.load(f)
        except json.JSONDecodeError:
            return {"users": {}}


def _save_users(data: dict) -> None:
    """
    Persiste o dicionário de utilizadores em users.json de forma atómica.
    A escrita é feita num ficheiro temporário seguida de os.replace(), evitando
    corrupção em caso de crash a meio da escrita.
    """
    os.makedirs("keys", exist_ok=True)
    tmp_path = USERS_JSON + ".tmp"
    with open(tmp_path, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=4, ensure_ascii=False)
    os.replace(tmp_path, USERS_JSON)


def _user_exists_in_json(username: str) -> bool:
    """
    Fonte única de verdade para a existência de uma conta local: a entrada
    correspondente no users.json. Os ficheiros .pem são estado derivado e
    podem ser regenerados; o hash da password é a âncora da identidade local.
    """
    with _users_json_lock:
        return username in _load_users().get("users", {})


def _keys_on_disk(username: str) -> bool:
    """Devolve True se os ficheiros de chaves do utilizador existem em disco."""
    key_dir = "keys"
    return (
        os.path.exists(os.path.join(key_dir, f"{username}_priv_ed.pem")) and
        os.path.exists(os.path.join(key_dir, f"{username}_priv_x.pem")) and
        os.path.exists(os.path.join(key_dir, f"{username}_pub.pem"))
    )


def _remove_local_key_files(username: str) -> None:
    """Apaga todos os ficheiros .pem locais de um utilizador (se existirem)."""
    for suffix in ["_priv_ed.pem", "_pub.pem", "_priv_x.pem", "_cert.pem"]:
        path = os.path.join("keys", f"{username}{suffix}")
        if os.path.exists(path):
            os.remove(path)


def _save_password_hash(username: str, password: str) -> None:
    """Cria ou atualiza o hash Argon2id do utilizador em users.json."""
    with _users_json_lock:
        data = _load_users()
        data.setdefault("users", {})[username] = {
            "password_hash": _ph.hash(password)
        }
        _save_users(data)


def _verify_password(username: str, password: str) -> bool:
    """
    Verifica a password contra o hash Argon2id guardado em users.json.
    Aplica rehash transparente se os parâmetros de custo mudaram.
    """
    with _users_json_lock:
        data = _load_users()
        user_entry = data.get("users", {}).get(username)
        if not user_entry:
            return False
        stored_hash = user_entry.get("password_hash", "")
        try:
            _ph.verify(stored_hash, password)
            if _ph.check_needs_rehash(stored_hash):
                data["users"][username]["password_hash"] = _ph.hash(password)
                _save_users(data)
            return True
        except (VerifyMismatchError, VerificationError, InvalidHashError):
            return False


def _remove_user_from_json(username: str) -> None:
    """Remove o registo do utilizador de users.json."""
    with _users_json_lock:
        data = _load_users()
        data.get("users", {}).pop(username, None)
        _save_users(data)


# ──────────────────────────────────────────────────────────────
# AUTENTICAÇÃO LOCAL
# ──────────────────────────────────────────────────────────────

def login() -> str:
    """
    Apresenta o ecrã de login/registo e devolve o username autenticado.

    A fonte única de verdade para "esta conta existe localmente" é o
    ficheiro users.json. Os ficheiros .pem das chaves são estado derivado:
    podem ser regenerados após uma autenticação válida.

    Casos tratados:
    - Utilizador presente em users.json com chaves em disco: login normal.
    - Utilizador presente em users.json mas sem chaves: após verificar a
      password, as chaves são regeneradas e o utilizador será re-registado
      no servidor com as novas chaves públicas.
    - Utilizador ausente de users.json mas com chaves órfãs em disco: as
      chaves antigas são removidas (estado inconsistente) e a conta é
      criada como nova.
    - Utilizador novo: pede password com confirmação e cria a entrada.

    Volta a pedir as credenciais indefinidamente até um login bem-sucedido
    ou até o utilizador sair com Ctrl+C.
    """
    print("\n=== Sistema de Chat E2EE ===\n")
    while True:
        username = input("Username: ").strip()
        if not username:
            print("[-] Username inválido. Tenta novamente.\n")
            continue

        if _user_exists_in_json(username):
            # ── Utilizador existente ──────────────────────────────────────────
            password = getpass.getpass("Password: ")
            if not _verify_password(username, password):
                print("[-] Password incorreta. Tenta novamente.\n")
                continue

            # Estado inconsistente: hash presente mas chaves em falta.
            # A password verificou, portanto a identidade é legítima — basta
            # regenerar as chaves derivadas.
            if not _keys_on_disk(username):
                print("[!] Chaves locais em falta — vão ser regeneradas.")
                print("[!] As chaves anteriores deixarão de ser válidas no servidor.")
                _remove_local_key_files(username)  # limpa eventuais ficheiros parciais

            print(f"[+] Autenticado como '{username}'.")
            return username
        else:
            # ── Novo utilizador ───────────────────────────────────────────────
            # Se houver ficheiros .pem órfãos para este username (sem entrada
            # correspondente em users.json), são restos de um estado anterior
            # inconsistente e devem ser removidos antes de criar a nova conta.
            if _keys_on_disk(username):
                print(f"[!] Encontradas chaves antigas para '{username}' sem conta associada.")
                confirm = input("Apagar essas chaves e criar uma nova conta? (s/n): ").strip().lower()
                if confirm != 's':
                    print("[-] Operação cancelada.\n")
                    continue
                _remove_local_key_files(username)
                print("[+] Chaves antigas removidas.")

            print(f"[*] Nenhuma conta local encontrada para '{username}'.")
            print("[*] Vais criar uma nova conta. Escolhe uma password segura.")
            password = getpass.getpass("Nova password: ")
            if not password:
                print("[-] A password não pode ser vazia. Tenta novamente.\n")
                continue
            password_confirm = getpass.getpass("Confirmar password: ")
            if password != password_confirm:
                print("[-] As passwords não coincidem. Tenta novamente.\n")
                continue
            _save_password_hash(username, password)
            print(f"[+] Password definida. A criar conta para '{username}'...")
            return username


def _reset_session_state() -> None:
    """
    Limpa todo o estado em memória relativo à sessão anterior,
    permitindo que um novo login arranque com um estado limpo.
    """
    global CLIENT_PRIV_ED, CLIENT_PUB_ED, CLIENT_PRIV_X, CLIENT_PUB_X
    global SERVER_PUB_ED, _username

    CLIENT_PRIV_ED = None
    CLIENT_PUB_ED = None
    CLIENT_PRIV_X = None
    CLIENT_PUB_X = None
    SERVER_PUB_ED = None
    _username = ""

    with _key_cache_lock:
        _e2e_key_cache.clear()

    # Esvaziar a fila de respostas residuais
    while not _response_queue.empty():
        try:
            _response_queue.get_nowait()
        except queue.Empty:
            break


# ──────────────────────────────────────────────────────────────
# GESTÃO DE CHAVES CRIPTOGRÁFICAS
# ──────────────────────────────────────────────────────────────

def load_or_generate_keys(username: str):
    """Carrega ou gera as chaves do cliente (Identidade Ed25519 e Troca X25519)."""
    global CLIENT_PRIV_ED, CLIENT_PUB_ED, CLIENT_PRIV_X, CLIENT_PUB_X, SERVER_PUB_ED

    key_dir = "keys"
    os.makedirs(key_dir, exist_ok=True)

    priv_ed_path = os.path.join(key_dir, f"{username}_priv_ed.pem")
    priv_x_path = os.path.join(key_dir, f"{username}_priv_x.pem")
    pub_ed_path = os.path.join(key_dir, f"{username}_pub.pem")

    # Identidade (Ed25519)
    if os.path.exists(priv_ed_path):
        with open(priv_ed_path, "rb") as f:
            CLIENT_PRIV_ED = cu.load_private_key_from_pem(f.read())
        CLIENT_PUB_ED = CLIENT_PRIV_ED.public_key()
    else:
        CLIENT_PRIV_ED, CLIENT_PUB_ED = cu.generate_ed25519_keypair()
        with open(priv_ed_path, "wb") as f:
            f.write(cu.private_key_to_pem(CLIENT_PRIV_ED))
        with open(pub_ed_path, "wb") as f:
            f.write(cu.public_key_to_pem(CLIENT_PUB_ED))

    # E2EE (X25519)
    if os.path.exists(priv_x_path):
        with open(priv_x_path, "rb") as f:
            CLIENT_PRIV_X = cu.load_private_key_from_pem(f.read())
        CLIENT_PUB_X = CLIENT_PRIV_X.public_key()
    else:
        CLIENT_PRIV_X, CLIENT_PUB_X = cu.generate_x25519_keypair()
        with open(priv_x_path, "wb") as f:
            f.write(cu.private_key_to_pem(CLIENT_PRIV_X))

    # Chave Pública do Servidor (necessária para o Handshake)
    server_pub_path = os.path.join(key_dir, "server_pub.pem")
    if not os.path.exists(server_pub_path):
        print("[-] ERRO: Chave pública do servidor não encontrada.")
        print("[-] Corre primeiro o servidor para gerar as chaves partilhadas.")
        sys.exit(1)

    with open(server_pub_path, "rb") as f:
        SERVER_PUB_ED = cu.load_public_key_from_pem(f.read())

    print("[+] Chaves locais carregadas com sucesso.")


# ──────────────────────────────────────────────────────────────
# E2EE — DERIVAÇÃO E CACHE DE CHAVES
# ──────────────────────────────────────────────────────────────

def compute_e2e_key(peer_pub_x_raw: bytes) -> bytes:
    """Calcula a chave simétrica E2EE usando a nossa chave privada X25519 e a pública do peer."""
    peer_pub_x = cu.load_x25519_public_key_from_raw(peer_pub_x_raw)
    shared_secret = cu.compute_shared_secret(CLIENT_PRIV_X, peer_pub_x)
    return cu.derive_key(shared_secret, info=b"chat_e2ee")


def get_or_fetch_e2e_key(sender: str, secure_channel: prot.SecureChannel) -> Optional[bytes]:
    """
    Devolve a chave E2E para 'sender' a partir da cache local.
    Se não estiver em cache, vai buscar ao servidor e verifica o certificado.
    Devolve None se a verificação falhar.
    """
    with _key_cache_lock:
        if sender in _e2e_key_cache:
            return _e2e_key_cache[sender]

    secure_channel.send({"action": "GET_KEY", "target": sender, "_reply_to": "tmp"})
    try:
        resp = _response_queue.get(timeout=10)
    except queue.Empty:
        print(f"\n[-] Timeout ao obter chave de '{sender}'.")
        return None

    if resp.get("status") != "OK":
        print(f"\n[-] Não foi possível obter chave de '{sender}': {resp.get('msg')}")
        return None

    sender_x_raw = prot.decode_base64(resp["keys"]["x25519_pub"])

    sender_cert_b64 = resp.get("certificate")
    if not sender_cert_b64 or not verify_peer_certificate(sender_cert_b64, sender):
        print(f"\n[-] SEGURANÇA: Identidade de '{sender}' não verificada. Mensagem rejeitada.")
        return None

    e2e_key = compute_e2e_key(sender_x_raw)

    with _key_cache_lock:
        _e2e_key_cache[sender] = e2e_key

    return e2e_key


def verify_peer_certificate(cert_b64: str, expected_username: str) -> bool:
    """
    Verifica se o certificado X.509 do peer foi emitido pela CA local e
    pertence ao utilizador esperado.
    """
    ca_cert_path = os.path.join("keys", "ca_cert.pem")
    if not os.path.exists(ca_cert_path):
        print("[-] AVISO: Certificado da CA não encontrado localmente. Verificação ignorada.")
        return False

    try:
        with open(ca_cert_path, "rb") as f:
            ca_cert_pem = f.read()

        cert_pem = prot.decode_base64(cert_b64)

        if not cu.verify_certificate_chain(cert_pem, ca_cert_pem):
            print(f"[-] SEGURANÇA: Certificado de '{expected_username}' NÃO foi assinado pela CA!")
            return False

        cn = cu.extract_username_from_certificate(cert_pem)
        if cn != expected_username:
            print(f"[-] SEGURANÇA: CN do certificado ('{cn}') não corresponde ao utilizador esperado ('{expected_username}')!")
            return False

        return True
    except Exception as e:
        print(f"[-] Erro ao verificar certificado de '{expected_username}': {e}")
        return False


# ──────────────────────────────────────────────────────────────
# HISTÓRICO LOCAL
# ──────────────────────────────────────────────────────────────

def save_to_history(username: str, sender: str, timestamp: str, content: str):
    """
    Guarda uma mensagem recebida no histórico local JSON.

    Protegido por _history_lock para evitar race entre o listener thread
    (push em tempo real via DELIVER_MSG) e o thread principal (pull via
    FETCH_MSGS). A escrita é atómica: ficheiro temporário + os.replace().
    """
    with _history_lock:
        data_dir = os.path.join("data", "history")
        os.makedirs(data_dir, exist_ok=True)

        history_file = os.path.join(data_dir, f"{username}_history.json")
        history = []
        if os.path.exists(history_file):
            with open(history_file, "r", encoding="utf-8") as hf:
                try:
                    history = json.load(hf)
                except json.JSONDecodeError:
                    pass

        history.append({"sender": sender, "timestamp": timestamp, "content": content})

        tmp_path = history_file + ".tmp"
        with open(tmp_path, "w", encoding="utf-8") as hf:
            json.dump(history, hf, indent=4, ensure_ascii=False)
        os.replace(tmp_path, history_file)


# ──────────────────────────────────────────────────────────────
# LISTENER THREAD (leitura assíncrona do socket)
# ──────────────────────────────────────────────────────────────

def listener_thread(secure_channel: prot.SecureChannel, username: str):
    """
    Corre em background e trata de toda a leitura do socket.

    - Mensagens DELIVER_MSG: decifradas e impressas imediatamente.
    - Tudo o resto: depositado em _response_queue para o thread principal.
    """
    while True:
        try:
            msg = secure_channel.recv()
        except Exception:
            break

        action = msg.get("action")

        if action == "DELIVER_MSG":
            sender = msg.get("sender", "?")
            timestamp = msg.get("timestamp", "?")

            with _key_cache_lock:
                e2e_key = _e2e_key_cache.get(sender)

            if e2e_key is None:
                print(f"\n[!] Mensagem de '{sender}' recebida mas chave E2E não está em cache.")
                print(f"[!] Usa 'fetch' para a ler (acontece apenas no primeiro contacto).")
                print(f"{username}> ", end="", flush=True)
                continue

            try:
                nonce = prot.decode_base64(msg["e2e_nonce"])
                ciphertext = prot.decode_base64(msg["e2e_ciphertext"])
                plaintext = cu.decrypt_aes_gcm(e2e_key, nonce, ciphertext).decode("utf-8")
                print(f"\n[{timestamp}] [E2EE] De {sender}: {plaintext}")
                print(f"{username}> ", end="", flush=True)
                save_to_history(username, sender, timestamp, plaintext)
            except ValueError:
                print(f"\n[-] Mensagem de {sender} com integridade comprometida! Rejeitada.")
                print(f"{username}> ", end="", flush=True)
        else:
            _response_queue.put(msg)


def wait_response(timeout: int = 10):
    """Bloqueia o thread principal até chegar uma resposta síncrona do servidor."""
    return _response_queue.get(timeout=timeout)


# ──────────────────────────────────────────────────────────────
# SESSÃO — ligação + REPL para um utilizador autenticado
# ──────────────────────────────────────────────────────────────

def run_session(username: str) -> bool:
    """
    Estabelece a ligação ao servidor, faz o handshake, regista as chaves
    e corre a REPL para o utilizador autenticado.

    Devolve:
        True  — o utilizador fez logout (volta ao ecrã de login).
        False — o utilizador saiu com 'quit' ou ocorreu erro fatal.
    """
    global _username
    _username = username

    # ── 1. Carregar / gerar chaves criptográficas ─────────────────────────────
    load_or_generate_keys(username)

    # ── 2. Ligar ao servidor ──────────────────────────────────────────────────
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        sock.connect((HOST, PORT))
    except ConnectionRefusedError:
        print("[-] Não foi possível ligar ao servidor.")
        return False

    # ── 3. Identificação inicial ──────────────────────────────────────────────
    prot.send_raw_msg(sock, {"username": username})

    # ── 4. Handshake Seguro ───────────────────────────────────────────────────
    print("[*] A estabelecer canal seguro...")
    try:
        secure_channel = prot.client_handshake(sock, CLIENT_PRIV_ED, SERVER_PUB_ED)
        print("[+] Canal seguro (mTLS-like) estabelecido com o Servidor.")
    except Exception as e:
        print(f"[-] Falha no Handshake: {e}")
        sock.close()
        return False

    # ── 5. Receber e armazenar o certificado da CA ────────────────────────────
    try:
        ca_msg = secure_channel.recv()
        if ca_msg.get("action") == "CA_CERT":
            ca_cert_pem = prot.decode_base64(ca_msg.get("ca_cert", ""))
            with open(os.path.join("keys", "ca_cert.pem"), "wb") as f:
                f.write(ca_cert_pem)
            print("[+] Certificado da CA armazenado localmente.")
        else:
            print("[-] AVISO: Certificado da CA não recebido.")
    except Exception as e:
        print(f"[-] Erro ao receber certificado da CA: {e}")
        sock.close()
        return False

    # ── 6. Registar as chaves no Servidor ─────────────────────────────────────
    pub_ed_b64 = prot.encode_base64(cu.public_key_to_raw(CLIENT_PUB_ED))
    pub_x_b64 = prot.encode_base64(cu.public_key_to_raw(CLIENT_PUB_X))

    secure_channel.send({
        "action": "REGISTER",
        "ed25519_pub": pub_ed_b64,
        "x25519_pub": pub_x_b64,
    })

    resp = secure_channel.recv()
    if resp.get("status") == "OK":
        print("[+] Registado no servidor com sucesso.")
        if resp.get("certificate"):
            client_cert_pem = prot.decode_base64(resp["certificate"])
            with open(os.path.join("keys", f"{username}_cert.pem"), "wb") as f:
                f.write(client_cert_pem)
            print("[+] Certificado do cliente armazenado localmente.")
    else:
        print(f"[-] Falha no registo: {resp.get('msg')}")
        sock.close()
        return False

    # ── 7. Lançar o listener thread ───────────────────────────────────────────
    t = threading.Thread(target=listener_thread, args=(secure_channel, username), daemon=True)
    t.start()

    print("\nComandos disponíveis:")
    print("  list            - Listar utilizadores registados")
    print("  send <user>     - Enviar mensagem (abre prompt)")
    print("  fetch           - Obter mensagens offline pendentes")
    print("  logout          - Terminar a sessão e voltar ao ecrã de login")
    print("  delete          - Eliminar a própria conta")
    print("  quit            - Sair da aplicação\n")

    # ── 8. REPL principal ─────────────────────────────────────────────────────
    do_logout = False
    try:
        while True:
            cmd_line = input(f"{username}> ").strip()
            if not cmd_line:
                continue

            parts = cmd_line.split()
            cmd = parts[0].lower()

            # QUIT
            if cmd == "quit":
                break

            # LOGOUT
            elif cmd == "logout":
                print(f"[*] A terminar sessão de '{username}'...")
                do_logout = True
                break

            # DELETE
            elif cmd == "delete":
                print("[!] Atenção: Esta ação irá eliminar a tua conta no servidor e apagar as chaves locais.")
                confirm = input("Tens a certeza? (s/n): ").strip().lower()
                if confirm == 's':
                    secure_channel.send({"action": "DELETE_USER"})
                    try:
                        resp = wait_response()
                        print(f"[*] {resp.get('msg')}")
                        if resp.get("status") == "OK":
                            for suffix in ["_priv_ed.pem", "_pub.pem", "_priv_x.pem", "_cert.pem"]:
                                path = os.path.join("keys", f"{username}{suffix}")
                                if os.path.exists(path):
                                    os.remove(path)
                            _remove_user_from_json(username)
                            print("[+] Dados locais eliminados.")
                    except queue.Empty:
                        print("[-] Sem resposta do servidor.")
                break

            # LIST
            elif cmd == "list":
                secure_channel.send({"action": "LIST_USERS"})
                try:
                    resp = wait_response()
                    users = resp.get("users", [])
                    print("[*] Utilizadores:", ", ".join(users))
                except queue.Empty:
                    print("[-] Sem resposta do servidor.")

            # SEND
            elif cmd == "send" and len(parts) == 2:
                target = parts[1]
                msg_text = input("Mensagem: ")

                with _key_cache_lock:
                    e2e_key = _e2e_key_cache.get(target)

                if e2e_key is None:
                    secure_channel.send({"action": "GET_KEY", "target": target})
                    try:
                        resp = wait_response()
                    except queue.Empty:
                        print("[-] Sem resposta do servidor.")
                        continue

                    if resp.get("status") != "OK":
                        print(f"[-] Erro: {resp.get('msg')}")
                        continue

                    target_keys = resp.get("keys")
                    peer_x_raw = prot.decode_base64(target_keys["x25519_pub"])

                    target_cert_b64 = resp.get("certificate")
                    if not target_cert_b64:
                        print(f"[-] SEGURANÇA: Sem certificado para '{target}'. Envio cancelado.")
                        continue
                    if not verify_peer_certificate(target_cert_b64, target):
                        print(f"[-] SEGURANÇA: Identidade de '{target}' não verificada. Envio cancelado.")
                        continue
                    print(f"[+] Identidade de '{target}' verificada pela CA.")

                    e2e_key = compute_e2e_key(peer_x_raw)
                    with _key_cache_lock:
                        _e2e_key_cache[target] = e2e_key

                plaintext = msg_text.encode("utf-8")
                nonce, ciphertext = cu.encrypt_aes_gcm(e2e_key, plaintext)

                secure_channel.send({
                    "action": "SEND_MSG",
                    "target": target,
                    "timestamp": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
                    "e2e_nonce": prot.encode_base64(nonce),
                    "e2e_ciphertext": prot.encode_base64(ciphertext),
                })
                try:
                    ack = wait_response()
                    print(f"[+] Enviado ({ack.get('msg')})")
                except queue.Empty:
                    print("[-] Sem confirmação do servidor.")

            # FETCH
            elif cmd == "fetch":
                secure_channel.send({"action": "FETCH_MSGS"})
                try:
                    resp = wait_response()
                except queue.Empty:
                    print("[-] Sem resposta do servidor.")
                    continue

                msgs = resp.get("messages", [])
                if not msgs:
                    print("[*] Não tens mensagens offline novas.")
                    continue

                for m in msgs:
                    sender = m["sender"]
                    nonce = prot.decode_base64(m["e2e_nonce"])
                    ciphertext = prot.decode_base64(m["e2e_ciphertext"])

                    with _key_cache_lock:
                        e2e_key = _e2e_key_cache.get(sender)

                    if e2e_key is None:
                        secure_channel.send({"action": "GET_KEY", "target": sender})
                        try:
                            k_resp = wait_response()
                        except queue.Empty:
                            print(f"[-] Timeout ao obter chave de '{sender}'.")
                            continue

                        if k_resp.get("status") != "OK":
                            print(f"[-] Não foi possível ler mensagem de {sender} (chave não encontrada).")
                            continue

                        sender_x_raw = prot.decode_base64(k_resp["keys"]["x25519_pub"])

                        sender_cert_b64 = k_resp.get("certificate")
                        if not sender_cert_b64 or not verify_peer_certificate(sender_cert_b64, sender):
                            print(f"[-] SEGURANÇA: Identidade de '{sender}' não verificada. Mensagem rejeitada.")
                            continue
                        print(f"[+] Identidade de '{sender}' verificada pela CA.")

                        e2e_key = compute_e2e_key(sender_x_raw)
                        with _key_cache_lock:
                            _e2e_key_cache[sender] = e2e_key

                    try:
                        plaintext = cu.decrypt_aes_gcm(e2e_key, nonce, ciphertext)
                        decrypted_msg = plaintext.decode("utf-8")
                        timestamp = m.get("timestamp", "Desconhecido")
                        print(f"\n[{timestamp}] [E2EE] De {sender}: {decrypted_msg}")
                        save_to_history(username, sender, timestamp, decrypted_msg)
                    except ValueError:
                        print(f"\n[-] Falha ao decifrar mensagem de {sender} (integridade comprometida!).")

            else:
                print("[-] Comando inválido.")

    except (ConnectionResetError, ConnectionAbortedError):
        print("\n[-] Ligação ao servidor perdida.")
    except KeyboardInterrupt:
        print("\n[*] A sair...")
        do_logout = False  # Ctrl+C encerra a aplicação
    finally:
        sock.close()

    return do_logout


# ──────────────────────────────────────────────────────────────
# PONTO DE ENTRADA
# ──────────────────────────────────────────────────────────────

def main():
    """
    Loop principal da aplicação.

    Apresenta o ecrã de login, corre a sessão e, se o utilizador fizer
    logout, repõe o estado e volta ao ecrã de login. O ciclo repete-se
    até um 'quit' ou Ctrl+C.
    """
    try:
        while True:
            username = login()
            do_logout = run_session(username)
            _reset_session_state()
            if not do_logout:
                break
            print("\n[*] Sessão terminada. Podes iniciar sessão com outra conta.\n")
    except KeyboardInterrupt:
        print("\n[*] A sair...")
    print("[*] Adeus.")


if __name__ == "__main__":
    main()