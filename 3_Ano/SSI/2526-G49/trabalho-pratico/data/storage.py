import json
import os
import threading
from typing import Dict, List, Optional, Any

# ==========================================
# ARMAZENAMENTO PERSISTENTE (SERVIDOR)
# ==========================================
# Justificação: O servidor é "honesto mas curioso". Ele precisa de armazenar
# chaves públicas e mensagens pendentes para garantir o funcionamento assíncrono
# do chat, mas NÃO PODE armazenar (nem consegue ler) o conteúdo das mensagens (plaintext).
# Este módulo garante a persistência opaca e a gestão de estado.

DATA_DIR = os.path.join("data", "history")
DB_FILE = os.path.join(DATA_DIR, "server_db.json")

# Estrutura base de dados em memória
# db_state = {
#     "users": {
#         "alice": {
#             "ed25519_pub": "base64...",
#             "x25519_pub": "base64..."
#         }
#     },
#     "messages": {
#         "alice": [
#             {
#                 "sender": "bob",
#                 "timestamp": 123456789,
#                 "e2e_nonce": "base64...",
#                 "e2e_ciphertext": "base64..."
#             }
#         ]
#     }
# }

_db_lock = threading.Lock()
_db_state = {"users": {}, "messages": {}}


def init_storage():
    """Inicializa o armazenamento, criando as diretorias e carregando dados existentes."""
    global _db_state
    if not os.path.exists(DATA_DIR):
        os.makedirs(DATA_DIR)

    if os.path.exists(DB_FILE):
        with open(DB_FILE, "r", encoding="utf-8") as f:
            try:
                _db_state = json.load(f)
            except json.JSONDecodeError:
                # Ficheiro corrompido, inicia vazio
                _db_state = {"users": {}, "messages": {}}
    else:
        _save_db()


def _save_db():
    """Guarda o estado atual da base de dados no disco de forma segura."""
    # Escreve primeiro num ficheiro temporário para evitar corrupção a meio da escrita
    tmp_file = DB_FILE + ".tmp"
    with open(tmp_file, "w", encoding="utf-8") as f:
        json.dump(_db_state, f, indent=4)
    os.replace(tmp_file, DB_FILE)


def register_user(username: str, ed25519_pub_b64: str, x25519_pub_b64: str) -> bool:
    """Regista ou atualiza um utilizador e as suas chaves públicas (identidade e troca)."""
    with _db_lock:
        _db_state["users"][username] = {
            "ed25519_pub": ed25519_pub_b64,
            "x25519_pub": x25519_pub_b64,
        }
        if username not in _db_state["messages"]:
            _db_state["messages"][username] = []
        _save_db()
        return True


def get_user_keys(username: str) -> Optional[Dict[str, str]]:
    """Devolve as chaves públicas de um utilizador, ou None se não existir."""
    with _db_lock:
        return _db_state["users"].get(username)


def list_users() -> List[str]:
    """Devolve a lista de utilizadores registados no servidor."""
    with _db_lock:
        return list(_db_state["users"].keys())


def store_message(
    recipient: str,
    sender: str,
    timestamp: Any,
    e2e_nonce_b64: str,
    e2e_ciphertext_b64: str,
) -> bool:
    """
    Armazena uma mensagem cifrada E2E para o destinatário.
    O servidor vê APENAS metadados (remetente, destinatário, hora) e os bytes opacos.
    """
    with _db_lock:
        if recipient not in _db_state["users"]:
            return False  # Destinatário não existe

        msg_record = {
            "sender": sender,
            "timestamp": timestamp,
            "e2e_nonce": e2e_nonce_b64,
            "e2e_ciphertext": e2e_ciphertext_b64,
        }

        if recipient not in _db_state["messages"]:
            _db_state["messages"][recipient] = []

        _db_state["messages"][recipient].append(msg_record)
        _save_db()
        return True


def get_and_clear_messages(username: str) -> List[Dict]:
    """Recupera e apaga as mensagens pendentes de um utilizador (simula 'entregue')."""
    with _db_lock:
        if username not in _db_state["messages"]:
            return []

        messages = _db_state["messages"][username]
        _db_state["messages"][username] = []  # Limpa a caixa de correio

        if messages:
            _save_db()

        return messages


def delete_user(username: str) -> bool:
    """Remove um utilizador e as suas mensagens da base de dados."""
    with _db_lock:
        if username in _db_state["users"]:
            del _db_state["users"][username]
            if username in _db_state["messages"]:
                del _db_state["messages"][username]
            _save_db()
            return True
        return False
