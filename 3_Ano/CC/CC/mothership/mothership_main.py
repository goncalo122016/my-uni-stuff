import asyncio
import argparse
import signal
import sys
import threading
import time
from typing import Optional
from common.message_types import Area

from common.utils import setup_logging
from common.protocol_config import ProtocolConfig
from mission_link.mission_protocol import MissionLinkProtocol
from mission_link.mission_manager import MissionManager
from telemetry.telemetry_manager import TelemetryManager
from telemetry.telemetry_stream import TelemetryStreamServer
from api.api_server import APIServer
from common.message_types import RoverState

class MotherShip:
    def __init__(self, host: str = None, api_host: str = None):
        self.host = host
        self.api_host = api_host

        self.logger = setup_logging("MotherShip")
        self.running = False
        self.show_telemetry = False
        
        # Managers
        self.mission_manager = MissionManager()
        self.telemetry_manager = TelemetryManager()
        
        # Servers
        self.mission_link_server: Optional[asyncio.DatagramTransport] = None
        self.telemetry_server: Optional[TelemetryStreamServer] = None
        self.api_server: Optional[APIServer] = None
        
        # Event loop
        self.loop: Optional[asyncio.AbstractEventLoop] = None
        
        self._setup_signal_handlers()
        self._setup_callbacks()
    
    def _setup_signal_handlers(self):
        """Configura handlers para sinais do sistema"""
        def signal_handler(signum, frame):
            self.logger.info(f"Sinal {signum} recebido, encerrando...")
            self.stop()
        
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)
    
    def _setup_callbacks(self):
        """Configura callbacks entre componentes"""
        # Mission callbacks
        self.mission_manager.on_mission_assigned = self._on_mission_assigned
        self.mission_manager.on_mission_completed = self._on_mission_completed
        self.telemetry_manager.on_high_temperature = self._on_high_temperature
        
        # Telemetry callbacks
        self.telemetry_manager.on_rover_connected = self._on_rover_connected
        self.telemetry_manager.on_low_battery = self._on_low_battery
    
    def _on_mission_assigned(self, mission):
        """Callback quando missão é atribuída"""
        self.logger.info(f"[ML] Missão {mission.mission_id} atribuída ao rover {mission.rover_id}")
    
    def _on_mission_completed(self, mission):
        """Callback quando missão é completada"""
        self.logger.info(f"[ML] Missão {mission.mission_id} completada pelo rover {mission.rover_id}")
    
    def _on_rover_connected(self, rover_id):
        """Callback quando rover conecta"""
        self.logger.info(f"[TS] Rover {rover_id} conectado ao sistema de telemetria")
    
    def _on_low_battery(self, rover_id, battery_level):
        """Callback quando bateria está baixa"""
        self.logger.info(f"ALERTA: Rover {rover_id} com bateria baixa: {battery_level}%")

    def _on_high_temperature(self, rover_id, temperature):
        """Callback quando a temperatura fica elevada"""
        # aviso normal
        self.logger.warning(
            f"ALERTA: Rover {rover_id} com temperatura elevada: {temperature:.1f}ºC"
        )


    async def start_mission_link_server(self):
        try:
            self.loop = asyncio.get_running_loop()

            protocol = MissionLinkProtocol(
                self.mission_manager,
                is_mothership=True,
                telemetry_manager=self.telemetry_manager
            )

            self.mission_manager.set_protocol(protocol)

            # Iniciar servidor UDP Mission Link
            transport, proto_instance = await self.loop.create_datagram_endpoint(
                lambda: protocol,
                local_addr=(self.host, ProtocolConfig.MISSION_LINK_PORT)
            )

            self.mission_link_server = transport

        except Exception as e:
            self.logger.error(f"Erro ao iniciar Mission Link Server: {e}")
            raise
    
    async def start_telemetry_server(self):
        """Inicia servidor Telemetry Stream TCP"""
        try:
            # Iniciar servidor Telemetry Stream TCP
            self.telemetry_server = TelemetryStreamServer(
                self.telemetry_manager,
                mothership=self,
                host=self.host,
                port=ProtocolConfig.TELEMETRY_STREAM_PORT
            )
            
            # Executar em thread separada para não bloquear
            def run_telemetry_server():
                asyncio.run(self.telemetry_server.start_server())
            
            telemetry_thread = threading.Thread(target=run_telemetry_server, daemon=True)
            telemetry_thread.start()
            
        except Exception as e:
            self.logger.error(f"Erro ao iniciar Telemetry Server: {e}")
            raise
    
    def start_api_server(self):
        """Inicia servidor API HTTP"""
        try:
            self.api_server = APIServer(
                self.telemetry_manager,
                self.mission_manager,
                host=self.api_host,
                port=ProtocolConfig.API_HTTP_PORT
            )
            
            self.api_server.start_server()
            
        except Exception as e:
            self.logger.error(f"Erro ao iniciar API Server: {e}")
            raise
    
    async def run_interactive_menu(self):
        while self.running:
            try:
                print("\n" + "="*60)
                print("           MOTHERSHIP CONTROL CENTER")
                print("="*60)
                print("1. Criar nova missão")
                print("2. Listar rovers conectados") 
                print("3. Listar missões ativas")
                print("4. Cancelar missão")
                print("5. Exibir/ocultar telemetria")
                print("6. Estatísticas do sistema")
                print("0. Sair")
                print("="*60)
                
                choice = await asyncio.get_event_loop().run_in_executor(
                    None, input, "Escolha uma opção: "
                )
                
                await self._handle_menu_choice(choice.strip())
                
            except KeyboardInterrupt:
                break
            except Exception as e:
                self.logger.error(f"Erro no menu interativo: {e}")
    
    async def _handle_menu_choice(self, choice):
        if choice == '1':
            await self._create_mission_interactive()
        elif choice == '2':
            self._list_connected_rovers()
        elif choice == '3':
            self._list_active_missions()
        elif choice == '4':
            await self._cancel_mission_interactive()
        elif choice == '5':
            self._toggle_telemetry_display()
        elif choice == '6':
            self._show_system_statistics()
        elif choice == '0':
            self.stop()
        else:
            print("Opção inválida!")
    
    async def _create_mission_interactive(self):
        try:
            print("\n--- CRIAR NOVA MISSÃO ---")

            x1 = float(input("Área X1: "))
            y1 = float(input("Área Y1: "))
            x2 = float(input("Área X2: "))
            y2 = float(input("Área Y2: "))

            tasks = input("Tarefas: ")
            duration = int(input("Duração (segundos): "))
            progress_period = int(input(
                f"Período de progresso (segundos, default {ProtocolConfig.DEFAULT_PROGRESS_PERIOD}): "
            ) or ProtocolConfig.DEFAULT_PROGRESS_PERIOD)

            area = Area(x1=x1, y1=y1, x2=x2, y2=y2)

            rover_id = input("Atribuir imediatamente a um Rover (deixe vazio): ").strip()
            if rover_id:
                mission_id = self.mission_manager.create_mission(area, tasks, duration, progress_period)
                if self.mission_manager.assign_mission(mission_id, rover_id):
                    print(f"Missão {mission_id} atribuída ao rover {rover_id}")
                else:
                    print("Falha ao atribuir missão.")
            else:
                # Criar missão sem atribuir
                mission_id = self.mission_manager.create_mission(area, tasks, duration, progress_period)

        except Exception as e:
            print(f"Erro ao criar missão: {e}")
    
    def _list_connected_rovers(self):
        rovers = self.telemetry_manager.get_all_rovers()
        
        if not rovers:
            print("Nenhum rover conectado.")
            return
        
        print("\n--- ROVERS CONECTADOS ---")
        for rover_id, telemetry in rovers.items():
            mission = self.mission_manager.get_rover_mission(rover_id)
            print(f"Rover: {rover_id}")
            print(f"  Estado: {telemetry.state.name}")
            print(f"  Posição: {telemetry.position.to_dict()}")
            print(f"  Bateria: {telemetry.battery}%")
            print(f"  Temperatura: {telemetry.temperature}°C")
            print(f"  Missão: {mission.mission_id if mission else 'Nenhuma'}")
            print("-" * 40)
    
    def _list_active_missions(self):
        missions = self.mission_manager.get_active_missions()
        
        if not missions:
            print("Nenhuma missão ativa.")
            return
        
        print("\n--- MISSÕES ATIVAS ---")
        for mission_id, mission in missions.items():
            print(f"Missão: {mission_id}")
            print(f"  Rover: {mission.rover_id}")
            print(f"  Tarefas: {mission.tasks}")
            print(f"  Progresso: {mission.progress:.1%}")
            print(f"  Status: {mission.status.value}")
            print("-" * 40)
    
    async def _cancel_mission_interactive(self):
        try:
            missions = self.mission_manager.get_active_missions()
    
            if not missions:
                print("Nenhuma missão ativa para cancelar.")
                return
    
            print("\n--- MISSÕES ATIVAS ---")
            for mid, m in missions.items():
                print(f"{mid}: Rover {m.rover_id} - {m.tasks}")
    
            mission_id = input("ID da missão para cancelar: ").strip()
    
            if self.mission_manager.cancel_mission(mission_id):
                print(f"Missão {mission_id} cancelada!")
            else:
                print("Erro ao cancelar missão.")
    
        except Exception as e:
            print(f"Erro: {e}")

    def _toggle_telemetry_display(self):
        self.show_telemetry = not self.show_telemetry
        state = "ATIVADO" if self.show_telemetry else "DESATIVADO"
        print(f"[TS] Print de telemetria: {state}")
    
    def _show_system_statistics(self):
        rovers = self.telemetry_manager.get_all_rovers()
        missions = self.mission_manager.get_all_missions()
        active_missions = self.mission_manager.get_active_missions()
        
        print("\n--- ESTATÍSTICAS DO SISTEMA ---")
        print(f"Rovers conectados: {len(rovers)}")
        print(f"Total de missões: {len(missions)}")
        print(f"Missões ativas: {len(active_missions)}")
        
        # Estatísticas por estado
        state_count = {}
        for telemetry in rovers.values():
            state_name = telemetry.state.name
            state_count[state_name] = state_count.get(state_name, 0) + 1

        print("Rovers por estado:")
        for state_name, count in state_count.items():
            print(f"  {state_name}: {count}")
            
    
    async def start(self):
        """Inicia todos os serviços da MotherShip"""
        self.running = True
        self.logger.info("Iniciando MotherShip...")
        
        try:
            # Iniciar serviços
            await self.start_mission_link_server()
            await self.start_telemetry_server()
            self.start_api_server()
            
            await asyncio.sleep(1)
            await self.run_interactive_menu()
            
        except Exception as e:
            self.logger.error(f"Erro ao iniciar MotherShip: {e}")
            self.stop()
    
    def stop(self):
        """Para todos os serviços"""
        self.running = False
        self.logger.info("Encerrando MotherShip...")
        
        # Fechar servidores
        if self.mission_link_server:
            self.mission_link_server.close()
        
        if self.api_server:
            self.api_server.stop_server()
        
        self.logger.info("MotherShip encerrada.")
        sys.exit(0)

async def main():
    parser = argparse.ArgumentParser(description="MotherShip Control Center")
    parser.add_argument("--host", default=None, help="IP for Mission Link and Telemetry Stream")
    parser.add_argument("--api-host", default=None, help="API Host IP address")
    args = parser.parse_args()
    
    mothership = MotherShip(host=args.host, api_host=args.api_host)
    await mothership.start()

if __name__ == "__main__":
    asyncio.run(main())