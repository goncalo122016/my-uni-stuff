import argparse
import requests
import json
import time
from common.utils import setup_logging
from common.protocol_config import ProtocolConfig

class GroundControl:
    def __init__(self, host: str, mothership_host: str):
        self.host = host
        self.mothership_host = mothership_host
        self.api_base_url = f"http://{mothership_host}:{ProtocolConfig.API_HTTP_PORT}"
        self.logger = setup_logging("GroundControl")

    # JSON REQUEST HANDLER
    def _request_json(self, method: str, path: str, **kwargs):
        # Sends an HTTP request and handles JSON responses
        url = f"{self.api_base_url}{path}"
        try:
            resp = requests.request(method, url, timeout=10, **kwargs)

            if resp.status_code >= 400:
                print(f"Erro HTTP {resp.status_code} em {path}")
                return None

            if not resp.content:
                print("Resposta vazia")
                return None

            return resp.json()
        except Exception as e:
            print(f"Erro na comunicação: {e}")
            return None

    # API operations
    def send_mission(self, rover_id: str, area: dict, tasks: str, duration: int, progress_period: int = 120) -> str:
        payload = {
            "rover_id": rover_id,
            "area": area,
            "tasks": tasks,
            "duration": duration,
            "progress_period": progress_period
        }
        evt = self._request_json("POST", "/send-mission", json=payload)

        if evt and evt["event"] in ("mission_assigned", "ok"):
            print(f"Missão enviada: {evt['data'].get('mission_id')}")
            return evt["data"].get("mission_id")
        return None

    def get_rovers(self):
        evt = self._request_json("GET", "/rovers")
        if evt and evt["event"] in ("rovers_snapshot", "telemetry"):
            return evt["data"].get("rovers", {})
        return {}

    def get_missions(self):
        evt = self._request_json("GET", "/missions")
        if evt and evt["event"] == "missions_snapshot":
            return evt["data"].get("missions", {})
        return {}

    def cancel_mission(self, mission_id: str):
        evt = self._request_json("POST", f"/missions/{mission_id}/cancel")
        if evt and evt["event"] != "error":
            print(f"Missão {mission_id} cancelada")
            return True
        print("Erro ao cancelar missão")
        return False

    def run_interactive_menu(self):
        while True:
            try:
                print(f"\n{'='*60}")
                print("           GROUND CONTROL CENTER")
                print(f"{'='*60}")
                print("1. Listar rovers")
                print("2. Listar missões")
                print("3. Enviar nova missão")
                print("4. Cancelar missão")
                print("5. Monitorizar rovers (tempo real)")
                print("0. Sair")
                print(f"{'='*60}")

                choice = input("Escolha uma opção: ").strip()

                if choice == '1':
                    self._list_rovers()
                elif choice == '2':
                    self._list_missions()
                elif choice == '3':
                    self._send_mission_interactive()
                elif choice == '4':
                    self._cancel_mission_interactive()
                elif choice == '5':
                    self._monitor_rovers()
                elif choice == '0':
                    break
                else:
                    print("Opção inválida!")

            except KeyboardInterrupt:
                break
            except Exception as e:
                print(f"Erro: {e}")

    def _list_rovers(self):
        rovers = self.get_rovers()
        if not rovers:
            print("Nenhum rover conectado.")
            return

        print("\n--- ROVERS DISPONÍVEIS ---")
        for rover_id, data in rovers.items():
            print(f"Rover: {rover_id}")
            print(f"  Estado: {data['state']}")
            print(f"  Posição: {data['position']}")
            print(f"  Bateria: {data['battery']}%")
            print(f"  Temperatura: {data['temperature']}°C")
            print(f"  Missão: {data.get('mission_id') or 'Nenhuma'}")
            print("-" * 40)

    def _list_missions(self):
        missions = self.get_missions()
        if not missions:
            print("Nenhuma missão encontrada.")
            return

        print("\n--- MISSÕES ---")
        for mission_id, data in missions.items():
            print(f"Missão: {mission_id}")
            print(f"  Rover: {data['rover_id'] or 'Não atribuído'}")
            print(f"  Status: {data['status']}")
            print(f"  Tarefas: {data['tasks']}")
            print(f"  Progresso: {data['progress']:.1%}")
            print("-" * 40)

    def _send_mission_interactive(self):
        try:
            rovers = self.get_rovers()
            idle_rovers = {rid: d for rid, d in rovers.items() if d['state'] == 'IDLE'}
            if not idle_rovers:
                print("Nenhum rover disponível (idle).")
                return

            print("\n--- ROVERS DISPONÍVEIS ---")
            for rover_id in idle_rovers:
                print(f"- {rover_id}")

            rover_id = input("\nID do rover: ").strip()
            if rover_id not in idle_rovers:
                print("Rover não disponível!")
                return

            print("\n--- DEFINIR ÁREA ---")
            x1 = float(input("X1: "))
            y1 = float(input("Y1: "))
            x2 = float(input("X2: "))
            y2 = float(input("Y2: "))

            area = {"x1": x1, "y1": y1, "x2": x2, "y2": y2}
            tasks = input("Tarefas: ")
            duration = int(input("Duração (segundos): "))
            progress_period = int(input("Período do progresso (segundos): "))

            mission_id = self.send_mission(rover_id, area, tasks, duration, progress_period)
            if mission_id:
                print(f"Missão {mission_id} enviada com sucesso!")

        except Exception as e:
            print(f"Erro ao criar missão: {e}")

    def _cancel_mission_interactive(self):
        missions = self.get_missions()
        active_missions = {mid: d for mid, d in missions.items() if d['status'] == 'active'}
        if not active_missions:
            print("Nenhuma missão ativa para cancelar.")
            return

        print("\n--- MISSÕES ATIVAS ---")
        for mission_id, data in active_missions.items():
            print(f"{mission_id}: {data['rover_id']} - {data['tasks']}")

        mission_id = input("\nID da missão para cancelar: ").strip()
        if mission_id in active_missions:
            self.cancel_mission(mission_id)
        else:
            print("Missão não encontrada ou não ativa.")

    def _monitor_rovers(self):
        print("\n--- MONITORIZAÇÃO EM TEMPO REAL ---")
        print("Pressione Ctrl+C para parar...")
        try:
            while True:
                rovers = self.get_rovers()
                print(f"{'='*60}")
                print(f"MONITORIZAÇÃO - {time.strftime('%H:%M:%S')}")
                print(f"{'='*60}")
                for rover_id, data in rovers.items():
                    pos = data['position']
                    print(f"{rover_id}: {data['state']} | Pos: ({pos['x']:.1f}, {pos['y']:.1f}, {pos['z']:.1f}) | Vel : ({data['speed']['x']:.1f}, {data['speed']['y']:.1f}, {data['speed']['z']:.1f}) | Bat: {data['battery']}% | Temp: {data['temperature']:.1f}°C")
                time.sleep(5)
        except KeyboardInterrupt:
            print("\nMonitorização interrompida.")


def main():
    parser = argparse.ArgumentParser(description="Ground Control Center")
    parser.add_argument("--host", required=True, help="Ground Control IP")
    parser.add_argument("--mothership", required=True, help="MotherShip IP")
    args = parser.parse_args()

    gc = GroundControl(args.host, args.mothership)
    gc.run_interactive_menu()


if __name__ == "__main__":
    main()
