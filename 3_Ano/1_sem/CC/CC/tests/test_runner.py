import yaml
import time
import requests
import sys

API_URL = "http://10.0.8.20:5007"

def load_scenarios(path):
    with open(path, "r") as f:
        return yaml.safe_load(f)["tests"]

def create_mission(m):
    print(f"→ Criar missão para {m['rover']}")
    payload = {
        "rover_id": m["rover"],
        "area": m["area"],
        "tasks": m["tasks"],
        "duration": m["duration"],
        "progress_period": m["progress_period"]
    }

    r = requests.post(f"{API_URL}/send-mission", json=payload)

    try:
        print("Resposta:", r.json())
    except Exception:
        print("Resposta BRUTA (erro ao decodificar JSON):")
        print(r.text)
        raise

    return r.json().get("mission_id")

def cancel_mission(mid):
    print(f"→ Cancelar missão {mid}")
    r = requests.post(f"{API_URL}/missions/{mid}/cancel")
    try:
        print("Resposta:", r.json())
    except Exception:
        print("Resposta bruta:", r.text)

def execute_scenario(scenario):
    print("\n==============================")
    print(f"TESTE: {scenario['name']}")
    print("==============================")

    created = []

    # Criar missões definidas no cenário
    for m in scenario["missions"]:
        mid = create_mission(m)
        created.append((mid, m))

    # Tratamento de abortos/cancelamentos
    for (mid, m) in created:
        if "abort_after" in m:
            time.sleep(m["abort_after"])
            cancel_mission(mid)

def run():
    # Se passar um argumento → executar só aquele teste
    TARGET_TEST = int(sys.argv[1]) if len(sys.argv) > 1 else None

    tests = load_scenarios("tests/mission_scenarios.yaml")

    if TARGET_TEST:
        print(f"=== EXECUTAR APENAS TESTE #{TARGET_TEST} ===")

        if TARGET_TEST < 1 or TARGET_TEST > len(tests):
            print(f"ERRO: Não existe teste número {TARGET_TEST}!")
            sys.exit(1)

        execute_scenario(tests[TARGET_TEST - 1])
        return

    # Caso contrário → executa todos
    print("=== EXECUTAR TODOS OS TESTES ===")
    for scenario in tests:
        execute_scenario(scenario)

if __name__ == "__main__":
    run()
