import json
import os

USERS_FILE = "data/users.json"

def load_users() -> dict:
    """Carrega utilizadores registados do ficheiro JSON."""
    if os.path.exists(USERS_FILE):
        with open(USERS_FILE, "r") as f:
            return json.load(f)
    return {}

def save_users(users: dict):
    """Persiste utilizadores no ficheiro JSON."""
    with open(USERS_FILE, "w") as f:
        json.dump(users, f, indent=2)
