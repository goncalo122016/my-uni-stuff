import asyncio
import argparse
import signal
import sys
import random
import time
import math
import threading
from typing import Optional

from common.utils import setup_logging
from common.protocol_config import ProtocolConfig
from common.message_types import TelemetryMessage, RoverState, Position, Speed
from mission_link.mission_protocol import MissionLinkProtocol
from telemetry.telemetry_stream import TelemetryStreamClient
from .rover_state import RoverStateManager
from common.message_types import RoverState as RS

class Rover:
    def __init__(self, rover_id: str, host: str, mothership_host: str):
        self.rover_id = rover_id
        self.host = host
        self.mothership_host = mothership_host
        self.logger = setup_logging(f"Rover-{rover_id}")
        self.running = False
        
        # State manager
        self.state_manager = RoverStateManager(rover_id)
         # Communication
        self.mission_protocol: Optional[MissionLinkProtocol] = None
        self.telemetry_client: Optional[TelemetryStreamClient] = None
        # Tasks
        self.telemetry_task: Optional[asyncio.Task] = None
        self.mission_progress_task: Optional[asyncio.Task] = None
        #ponto carregamento
        self._charging_point = {"x": 50.0, "y": 50.0, "z": 0.0}
        self._mission_step_size_ = None
        # temperatura
        self._ambient_temp = 20.0  # “temperatura ambiente”
        self._move_heat_rate = 0.1  # quanto aquece por tick em movimento
        self._idle_cool_rate = 0.1  # quanto arrefece por tick em idle
        self._charging_cool_rate = 0.04  # metade da escala de idle
        self._overheated = False # proteção contra sobreaquecimento
        self._sensor_failures = {
            "tires": False,
            "antenna": False,
            "battery": False,
            "temperature": False,
        }
        self._setup_signal_handlers()

    def _get_target_center(self, area):
        """Calcula o centro da área de missão."""
        target_x = (area["x1"] + area["x2"]) / 2
        target_y = (area["y1"] + area["y2"]) / 2
        target_z = 0.0
        return target_x, target_y, target_z

    def _compute_progress(self, active_time: float, duration: float) -> float:
        """Calcula o progresso [0,1] com base no tempo ativo."""
        if duration <= 0:
            return 1.0
        return min(active_time / duration, 1.0)

    def _send_progress(self, mission_info, progress: float, state: dict):
        """Envia mensagem de progresso para a MotherShip."""
        if not self.mission_protocol:
            return

        mothership_addr = (self.mothership_host, ProtocolConfig.MISSION_LINK_PORT)
        pos = state["position"]

        self.logger.info(
            f"[ML] Enviando progresso da missão {mission_info['mission_id']}: "
            f"{progress:.1%}"
        )

        self.mission_protocol.send_progress(
            mission_info["mission_id"],
            progress,
            Position(**pos),
            state["battery"],
            mothership_addr,
        )

    def _tick_error_state(self, mission_info, rover_state: RoverState) -> bool:
        """
        Se o rover estiver em ERROR, aborta a missão e termina a simulação.
        Retorna True se a missão foi abortada (ciclo principal deve sair).
        """
        if rover_state != RoverState.ERROR:
            return False

        self.logger.warning(
            f"[ROVER] Estado ERROR detetado. A abortar missão {mission_info['mission_id']}."
        )

        if self.mission_protocol:
            mothership_addr = (self.mothership_host, ProtocolConfig.MISSION_LINK_PORT)
            self.mission_protocol.abort_mission(
                mission_info["mission_id"],
                mothership_addr
            )

        self.state_manager.complete_mission()
        return True

    async def _tick_overheat(
        self,
        mission_info,
        progress_period: float,
        duration: float,
        active_time: float,
    ) -> float:
        state_idle = self.state_manager.get_state()
        self._update_temperature_by_state(state_idle, speed_factor=0.0)
        state_idle = self.state_manager.get_state()
        temp = state_idle["temperature"]

        # progresso fica congelado
        progress = self._compute_progress(active_time, duration)
        self.state_manager.update_progress(progress)

        if self.mission_protocol:
            mothership_addr = (
                self.mothership_host,
                ProtocolConfig.MISSION_LINK_PORT,
            )
            pos = state_idle["position"]
            self.mission_protocol.send_progress(
                mission_info["mission_id"],
                progress,
                Position(**pos),
                state_idle["battery"],
                mothership_addr,
            )

        # verificar se já arrefeceu
        if temp <= 30.0:
            self._overheated = False
            if self.state_manager.has_active_mission():
                self.state_manager.set_state(RoverState.IN_MISSION)

        await asyncio.sleep(progress_period)
        return active_time  # não aumenta

    async def _tick_charging(
        self,
        mission_info,
        progress: float,
        progress_period: float,
    ):
        """
        Tick quando o rover está em CHARGING:
        - move-se/permanece no posto de carregamento
        - envia progresso mas não avança na missão
        """
        self._go_to_charging_station()

        if self.mission_protocol:
            mothership_addr = (
                self.mothership_host,
                ProtocolConfig.MISSION_LINK_PORT,
            )
            state = self.state_manager.get_state()
            self.mission_protocol.send_progress(
                mission_info["mission_id"],
                progress,
                Position(**state["position"]),
                state["battery"],
                mothership_addr,
            )

        await asyncio.sleep(progress_period)

    def _tick_active_phase_move(
        self,
        mission_info,
        duration: float,
        active_time: float,
        progress: float,
    ):
        """
        Fase de movimento da missão:
        - Fase 1: ir ao centro na primeira metade do tempo
        - Fase 2: já no centro -> executar tarefa
        """
        area = mission_info["area"]
        state = self.state_manager.get_state()
        pos = state["position"]

        target_x, target_y, target_z = self._get_target_center(area)

        dx = target_x - pos["x"]
        dy = target_y - pos["y"]
        dz = target_z - pos["z"]
        dist = math.sqrt(dx * dx + dy * dy + dz * dz)

        # FASE 1 – ir até ao centro em metade do tempo
        if progress < 0.5:
            remaining_time_to_target = max(0.1, (duration / 2.0) - active_time)

            progress_period = mission_info["progress_period"]
            if progress_period > 0:
                num_steps = max(1, int(remaining_time_to_target / progress_period))
            else:
                num_steps = 1

            step_size = dist / num_steps if num_steps > 0 else 0.0

            # atualizar progresso (baseado no tempo)
            self.state_manager.update_progress(progress)

            # mover em direção ao centro com passo constante
            self._simulate_movement(area, forced_speed=step_size)

            if dist < 0.5:
                self.state_manager.update_position(
                    {"x": target_x, "y": target_y, "z": target_z}
                )

        # FASE 2 – já chegou ao centro -> executar tarefa parado
        else:
            self.state_manager.update_position(
                {"x": target_x, "y": target_y, "z": target_z}
            )
            self._simulate_task_execution()
            self.state_manager.update_progress(progress)

    async def _tick_active_mission(
        self,
        mission_info,
        duration: float,
        active_time: float,
        progress_period: float,
    ) -> float:
        """
        Tick principal da missão em modo normal (não a carregar, não sobreaquecido).
        - aumenta active_time
        - trata movimento / execução de tarefa
        - envia progresso
        Retorna o novo active_time.
        """
        # progresso com base no tempo ativo
        progress = self._compute_progress(active_time, duration)

        # movimento/tarefa
        self._tick_active_phase_move(mission_info, duration, active_time, progress)

        # enviar progresso
        state_after = self.state_manager.get_state()
        self._send_progress(mission_info, progress, state_after)

        # completar missão se necessário
        if progress >= 1.0:
            await self._complete_mission()
            return active_time  # não interessa mais, vai sair no loop

        await asyncio.sleep(progress_period)
        return active_time

    
    def _setup_signal_handlers(self):
        """Configura handlers para sinais do sistema"""
        def signal_handler(signum, frame):
            self.logger.info(f"Sinal {signum} recebido, encerrando...")
            self.stop()
        
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)

    async def start_mission_protocol(self):
        """Inicia protocolo Mission Link"""
        try:
            # AQUI
            self.mission_protocol = MissionLinkProtocol(None, is_mothership=False)
            self.mission_protocol.rover_id = self.rover_id

            # Configurar callbacks
            self.mission_protocol.on_mission_received = self._on_mission_received
            self.mission_protocol.on_mission_cancelled = self._on_mission_cancelled

            # Criar endpoint UDP
            loop = asyncio.get_running_loop()
            transport, protocol = await loop.create_datagram_endpoint(
                lambda: self.mission_protocol,
                local_addr=(self.host, 0)  # Porta automática
            )

            # Enviar HELLO para MotherShip
            self.mission_protocol.send_hello(
                self.rover_id,
                (self.mothership_host, ProtocolConfig.MISSION_LINK_PORT)
            )

        except Exception as e:
            self.logger.error(f"Erro ao iniciar Mission Protocol: {e}")
            raise

    async def start_telemetry_client(self):
        try:
            self.telemetry_client = TelemetryStreamClient(
                self.rover_id,
                self.mothership_host,
                ProtocolConfig.TELEMETRY_STREAM_PORT
            )
            
            if await self.telemetry_client.connect():
                # Iniciar envio periódico
                self.telemetry_task = asyncio.create_task(
                    self.telemetry_client.start_periodic_telemetry(
                        self._get_current_telemetry
                    )
                )
            else:
                raise Exception("Falha ao conectar ao servidor de telemetria")
                
        except Exception as e:
            self.logger.error(f"Erro ao iniciar cliente de telemetria: {e}")
            raise

    def _check_sensors_and_update_state(self, state: dict):
        """Verifica os 4 sensores e, se algum falhar, mete o rover em ERROR."""

        try:
            rover_state = RS(state["rover_state"])
            battery = float(state["battery"])
            temp = float(state["temperature"])
        except Exception as e:
            self.logger.error(f"[ROVER] Erro ao ler estado para sensores: {e}")
            return
        fail = self._sensor_failures

        # TEMPERATURA: falha se estiver fora de [-20, 150] ºC
        if not fail["temperature"]:
            if temp < -20.0 or temp > 150.0:
                fail["temperature"] = True
                self.logger.warning(
                    f"[ROVER] Falha no sensor de temperatura (leitura: {temp:.1f}ºC)."
                )
        # BATERIA: falha se < 0%, > 100% ou <= 5%
        if not fail["battery"]:
            if battery < 0 or battery > 100:
                fail["battery"] = True
                self.logger.warning(
                    f"[ROVER] ERRO no sensor de bateria (fora de 0-100%, leitura: {battery:.1f}%)."
                )

        # PNEUS: falha se pressão fora de 0-73 psi
        tire_pressure = state.get("tire_pressure_psi", 32.0)
        if not fail["tires"]:
            try:
                tp = float(tire_pressure)
                if tp < 0 or tp > 73:
                    fail["tires"] = True
                    self.logger.warning(
                        f"[ROVER] Falha no sensor de ar dos pneus "
                        f"(pressão fora de gama: {tp:.1f} psi)."
                    )
            except Exception:
                fail["tires"] = True
                self.logger.error(
                    f"[ROVER] Erro ao ler pressão dos pneus: valor inválido '{tire_pressure}'."
                )

        # ANTENA: falha simulada com 1% de probabilidade, falha persistente
        if not fail["antenna"]:
            # 1% de probabilidade de falhar, falha persistente
            if random.random() < 0.001:
                fail["antenna"] = True
                self.logger.warning("[ROVER] Falha simulada no sensor da antena.")

        # Se algum falhou, entra em ERROR
        if any(fail.values()) and rover_state != RS.ERROR:
            self.state_manager.set_state(RoverState.ERROR)


    def _get_current_telemetry(self) -> TelemetryMessage:
        state = self.state_manager.get_state()

        # Se não há missão ativa, deixa a temperatura aproximar-se do ambiente
        if not self.state_manager.has_active_mission():
            self._update_temperature_by_state(state, speed_factor=0.0)
            state = self.state_manager.get_state()  # ler outra vez após update

        # Verificar sensores (pode meter o rover em ERROR)
        self._check_sensors_and_update_state(state)
        state = self.state_manager.get_state()  # voltar a ler caso o estado tenha mudado

        return TelemetryMessage(
            rover_id=self.rover_id,
            mission_id=state['current_mission_id'],
            position=Position(**state['position']),
            state=RoverState(state['rover_state']),
            battery=state['battery'],
            speed=Speed(**state['speed']),
            temperature=state['temperature'],
            sensor_flags=self._encode_sensor_flags()
        )

    
    def _on_mission_received(self, mission_message):
        """Callback quando missão é recebida"""
        self.logger.info(f"[ML] Nova missão recebida: {mission_message.mission_id}")
        
        # Atualizar estado
        self.state_manager.set_mission(
            mission_message.mission_id,
            {
                'mission_id': mission_message.mission_id,
                'area': mission_message.area.to_dict(),
                'tasks': mission_message.tasks,
                'duration': mission_message.duration,
                'progress_period': mission_message.progress_period
            }
        )
        
        # Iniciar simulação da missão
        self.mission_progress_task = asyncio.create_task(self._simulate_mission())
    
    def _on_mission_cancelled(self, mission_message):
        """Callback quando missão é cancelada"""
        self.logger.info(f"[ML] Missão {mission_message.mission_id} cancelada")
        
        # Parar missão
        if self.mission_progress_task:
            self.mission_progress_task.cancel()
        
        self.state_manager.complete_mission()

    async def _simulate_mission(self):
        try:
            mission_info = self.state_manager.get_mission_info()
            if not mission_info:
                return

            duration = mission_info["duration"]
            progress_period = mission_info["progress_period"]

            start_time = time.time()
            last_tick_time = start_time
            active_time = 0.0  # tempo efetivo de missão (não conta carregamento)

            while self.running and self.state_manager.has_active_mission():
                now = time.time()
                dt = now - last_tick_time
                last_tick_time = now

                state = self.state_manager.get_state()
                rover_state = RoverState(state["rover_state"])
                temp = state["temperature"]

                # 1) Se entrou em ERROR -> aborta missão e termina
                if self._tick_error_state(mission_info, rover_state):
                    break

                # 2) Se ainda não estava em overheat mas passou dos 100ºC -> ativa proteção
                if not self._overheated and temp >= 100.0:
                    self._overheated = True
                    self.state_manager.set_state(RoverState.IDLE)
                    rover_state = RoverState.IDLE

                # 3) Se está em modo sobreaquecido -> trata só arrefecimento e progresso congelado
                if self._overheated:
                    active_time = await self._tick_overheat(
                        mission_info,
                        progress_period,
                        duration,
                        active_time,
                    )
                    continue  # próximo ciclo

                # 4) Se não está a carregar, o tempo ativo aumenta
                if rover_state != RoverState.CHARGING:
                    active_time += dt

                # progresso **sempre** calculado pelo tempo ativo
                progress = self._compute_progress(active_time, duration)

                # 5) Em modo CHARGING -> vai ao posto, carrega, envia progresso, mas não avança missão
                if rover_state == RoverState.CHARGING:
                    await self._tick_charging(
                        mission_info,
                        progress,
                        progress_period,
                    )
                    continue

                # 6) Modo normal de missão (IN_MISSION, etc.)
                active_time = await self._tick_active_mission(
                    mission_info,
                    duration,
                    active_time,
                    progress_period,
                )

                # Se por qualquer razão a missão deixou de estar ativa, sai
                if not self.state_manager.has_active_mission():
                    break

        except asyncio.CancelledError:
            self.logger.info("Simulação da missão cancelada")
        except Exception as e:
            self.logger.error(f"Erro na simulação da missão: {e}")


    def _simulate_movement(self, area, forced_speed: float = None):
        state = self.state_manager.get_state()
        pos = state["position"]

        target_x = (area["x1"] + area["x2"]) / 2
        target_y = (area["y1"] + area["y2"]) / 2
        target_z = 0  # assume nivel do solo

        # Vetor direção
        dx = target_x - pos["x"]
        dy = target_y - pos["y"]
        dz = target_z - pos["z"]

        # Distância ao objetivo
        dist = (dx ** 2 + dy ** 2 + dz ** 2) ** 0.5

        if dist < 0.5:
            # Já está praticamente no destino -> quase parado
            new_speed = {"x": 0, "y": 0, "z": 0}
            self.state_manager.update_speed(new_speed)
            return

        # Se tivermos uma velocidade forçada (calculada para chegar ao alvo no tempo certo)
        if forced_speed is not None and forced_speed > 0:
            speed = min(dist, forced_speed)
            # fator só para efeitos de bateria/temperatura (comparado com 1.5 m/pass)
            speed_factor = min(1.0, speed / 1.5)
        else:
            # Comportamento antigo (fallback)
            base_speed = 1.5  # velocidade base
            speed_factor = max(0.2, min(1.0, dist / 20))
            speed = base_speed * speed_factor

        # Normalizar direção
        nx = dx / dist
        ny = dy / dist
        nz = dz / dist if dist != 0 else 0.0

        # Aplicar velocidade (step por tick)
        new_position = {
            "x": pos["x"] + nx * speed,
            "y": pos["y"] + ny * speed,
            "z": pos["z"] + nz * speed
        }

        new_speed = {"x": nx * speed, "y": ny * speed, "z": nz * speed}

        # Consumo de bateria realista
        battery_drain = 0.3 * speed_factor
        new_battery = max(0, state["battery"] - battery_drain)

        # Temperatura realista
        self._update_temperature_by_state(state, speed_factor)

        # Atualizar estado
        self.state_manager.update_position(new_position)
        self.state_manager.update_speed(new_speed)
        self.state_manager.update_battery(new_battery)

    def _go_to_charging_station(self):
        """Move o rover para o posto de carregamento (50, 50, 0) e carrega a bateria."""
        state = self.state_manager.get_state()
        pos = state["position"]
        battery = state["battery"]

        # Posto de carregamento FIXO
        target_x = self._charging_point["x"]  # 50.0
        target_y = self._charging_point["y"]  # 50.0
        target_z = self._charging_point["z"]  # 0.0

        dx = target_x - pos["x"]
        dy = target_y - pos["y"]
        dz = target_z - pos["z"]

        dist = (dx ** 2 + dy ** 2 + dz ** 2) ** 0.5

        # Se já está praticamente no posto -> carregar
        if dist < 0.5:
            new_battery = min(100.0, battery + 10.0)  # +10% por ciclo, ajusta se quiseres
            self.state_manager.update_battery(new_battery)
            self.logger.info(
                f"[ROVER] No posto de carregamento, bateria={new_battery:.1f}%"
            )

            if new_battery >= 100.0:
                self.logger.info(
                    "[ROVER] Carregamento completo. A retomar a missão."
                )
                self.state_manager.set_state(RoverState.IN_MISSION)
            return

        # Ainda a caminho do posto -> movimento semelhante ao da missão
        base_speed = 1.5
        speed_factor = max(0.2, min(1.0, dist / 20))
        speed = base_speed * speed_factor

        nx = dx / dist
        ny = dy / dist
        nz = dz / dist if dist != 0 else 0.0

        new_position = {
            "x": pos["x"] + nx * speed,
            "y": pos["y"] + ny * speed,
            "z": pos["z"] + nz * speed,
        }

        new_speed = {"x": nx * speed, "y": ny * speed, "z": nz * speed}

        # Pode gastar um bocadinho de bateria a caminho
        battery_drain = 0.02 * speed_factor
        new_battery = max(0.0, battery - battery_drain)

        self._update_temperature_by_state(state, speed_factor=0.0)
        self.logger.info(
            f"[ROVER] A caminho do posto de carregamento: "
            f"pos=({new_position['x']:.1f},{new_position['y']:.1f}), "
            f"bateria={new_battery:.1f}%"
        )

        self.state_manager.update_position(new_position)
        self.state_manager.update_speed(new_speed)
        self.state_manager.update_battery(new_battery)


    async def _complete_mission(self):
        """Completa missão atual"""
        mission_info = self.state_manager.get_mission_info()
        if mission_info and self.mission_protocol:
            mothership_addr = (self.mothership_host, ProtocolConfig.MISSION_LINK_PORT)
            
            self.mission_protocol.complete_mission(
                mission_info['mission_id'],
                mothership_addr
            )
            
            self.state_manager.complete_mission()
    
    async def run_interactive_menu(self):
        while self.running:
            try:
                print(f"\n{'='*50}")
                print(f"         ROVER {self.rover_id} CONTROL")
                print(f"{'='*50}")
                print("1. Solicitar missão")
                print("2. Status do rover")
                print("3. Abortar missão atual")
                print("4. Alterar configurações")
                print("5. Simular emergência")
                print("0. Sair")
                print(f"{'='*50}")
                
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
            await self._request_mission()
        elif choice == '2':
            self._show_status()
        elif choice == '3':
            await self._abort_mission()
        elif choice == '4':
            await self._change_configuration()
        elif choice == '5':
            self._simulate_emergency()
        elif choice == '0':
            self.stop()
        else:
            print("Opção inválida!")
    
    async def _request_mission(self):
        """Solicita missão à MotherShip"""
        if self.state_manager.has_active_mission():
            print("Rover já tem missão ativa!")
            return
        
        if self.mission_protocol:
            mothership_addr = (self.mothership_host, ProtocolConfig.MISSION_LINK_PORT)
            self.mission_protocol.request_mission(self.rover_id, mothership_addr)
            print("Pedido de missão enviado!")
    
    def _show_status(self):
        state = self.state_manager.get_state()
        mission_info = self.state_manager.get_mission_info()
        
        print(f"\n--- STATUS DO ROVER {self.rover_id} ---")
        print(f"Estado: {RoverState(state['rover_state']).name}")
        print(f"Posição: {state['position']}")
        print(f"Velocidade: {state['speed']}")
        print(f"Temperatura: {state['temperature']:.1f}°C")
        print(f"Bateria: {state['battery']:.1f}%")
        
        if mission_info:
            print(f"Missão: {mission_info['mission_id']}")
            print(f"Progresso: {state['mission_progress']:.1%}")
            print(f"Tarefas: {mission_info['tasks']}")
        else:
            print("Missão: Nenhuma")
    
    async def _abort_mission(self):
        mission_info = self.state_manager.get_mission_info()
        
        if not mission_info:
            print("Nenhuma missão ativa para abortar.")
            return
        
        if self.mission_protocol:
            mothership_addr = (self.mothership_host, ProtocolConfig.MISSION_LINK_PORT)
            self.mission_protocol.abort_mission(
                mission_info['mission_id'],
                mothership_addr
            )
            
            if self.mission_progress_task:
                self.mission_progress_task.cancel()
            
            self.state_manager.complete_mission()
            print(f"Missão {mission_info['mission_id']} abortada!")
    
    async def _change_configuration(self):
        print("\n--- ALTERAR CONFIGURAÇÕES ---")
        print("1. Posição")
        print("2. Bateria")
        print("3. Temperatura")
        print("4. Pressão pneus")
        print("5. Forçar falha sensor antena")
        print("6. Voltar")
        
        choice = input("Escolha: ").strip()
        
        if choice == '1':
            try:
                x = float(input("Nova posição X: "))
                y = float(input("Nova posição Y: "))
                z = float(input("Nova posição Z: "))
                self.state_manager.update_position({'x': x, 'y': y, 'z': z})
                print("Posição atualizada!")
            except ValueError:
                print("Valores inválidos!")
        elif choice == '2':
            try:
                battery = float(input("Novo nível de bateria (%): "))
                self.state_manager.update_battery(battery, clamp=False)
                print("Bateria atualizada!")
            except ValueError:
                print("Valor inválido!")
        elif choice == '3':
            try:
                temp = float(input("Nova temperatura (°C): "))
                # aqui podes limitar se quiseres, ex:
                # temp = max(-50.0, min(200.0, temp))
                self.state_manager.update_temperature(temp)
                print("Temperatura atualizada!")
            except ValueError:
                print("Valor inválido!")
        elif choice == '4':
            try:
                pressure = float(input("Nova pressão dos pneus (psi): "))
                self.state_manager.update_tire_pressure(pressure)
                print("Pressão dos pneus atualizada!")
            except ValueError:
                print("Valor inválido!")
        elif choice == '5':
            self._sensor_failures["antenna"] = True
            self.logger.warning("[ROVER] Erro forçado no sensor da antena via menu.")
            print("Sensor da antena marcado como NÃO FUNCIONAL (erro forçado).")
        elif choice == '6':
            # apenas volta ao menu anterior
            return
        else:
            print("Opção inválida!")
    
    def _simulate_emergency(self):
        """Simula situação de emergência"""
        print("\n--- SIMULAR EMERGÊNCIA ---")
        print("1. Bateria crítica (5%)")
        print("2. Erro no sistema")
        print("3. Voltar")
        
        choice = input("Escolha: ").strip()
        
        if choice == '1':
            self.state_manager.update_battery(5)
            self.state_manager.set_state(RoverState.ERROR)
            print("Emergência: Bateria crítica simulada!")
        
        elif choice == '2':
            self.state_manager.set_state(RoverState.ERROR)
            print("Emergência: Erro no sistema simulado!")
    
    async def start(self):
        """Inicia rover"""
        self.running = True
        self.logger.info(f"Iniciando Rover {self.rover_id}...")
        
        try:
            # Iniciar protocolos
            await self.start_mission_protocol()
            await self.start_telemetry_client()
            
            self.logger.info("Rover iniciado com sucesso!")
            
            await asyncio.sleep(1)
            await self.run_interactive_menu()
            
        except Exception as e:
            self.logger.error(f"Erro ao iniciar rover: {e}")
            self.stop()
    
    def stop(self):
        """Para rover"""
        self.running = False
        self.logger.info("Encerrando rover...")
        
        if self.telemetry_task:
            self.telemetry_task.cancel()
        
        if self.mission_progress_task:
            self.mission_progress_task.cancel()
        
        # Desconectar telemetria
        if self.telemetry_client:
            asyncio.create_task(self.telemetry_client.disconnect())
        
        self.logger.info("Rover encerrado.")
        sys.exit(0)


    def _simulate_task_execution(self):
        state = self.state_manager.get_state()

        # parado
        self.state_manager.update_speed({"x": 0.0, "y": 0.0, "z": 0.0})

        # consumo mais pequeno que em movimento
        battery_drain = 0.05
        new_battery = max(0.0, state["battery"] - battery_drain)

        # temperatura flutua ligeiramente
        new_temperature = state["temperature"] + random.uniform(-0.05, 0.05)

        self.state_manager.update_battery(new_battery)
        self._update_temperature_by_state(state, speed_factor=0.0)

    def _update_temperature_by_state(self, state, speed_factor: float = 0.0):
        temp = state["temperature"]
        rover_state = RS(state["rover_state"])

        # Movimento (IN_MISSION + a mexer-se)
        if rover_state == RS.IN_MISSION and speed_factor > 0.0:
            temp += self._move_heat_rate * speed_factor

        # Idle -> arrefece em direção à temperatura ambiente
        elif rover_state == RS.IDLE:
            delta = temp - self._ambient_temp
            temp -= self._idle_cool_rate * delta

        # Charging -> também arrefece mas mais devagar
        elif rover_state == RS.CHARGING:
            delta = temp - self._ambient_temp
            temp -= self._charging_cool_rate * delta

        # pequeno ruído para não ser totalmente “seco”
        temp += random.uniform(-0.05, 0.05)

        self.state_manager.update_temperature(temp)

    def _encode_sensor_flags(self) -> int:
        flags = 0
        # bits: 0=temp, 1=battery, 2=tires, 3=antenna
        if self._sensor_failures["temperature"]:
            flags |= 1 << 0
        if self._sensor_failures["battery"]:
            flags |= 1 << 1
        if self._sensor_failures["tires"]:
            flags |= 1 << 2
        if self._sensor_failures["antenna"]:
            flags |= 1 << 3
        return flags


async def main():
    parser = argparse.ArgumentParser(description="Rover Control System")
    parser.add_argument("--rover-id", required=True, help="Rover ID")
    parser.add_argument("--host", required=True, help="Rover host IP")
    parser.add_argument("--mothership", required=True, help="MotherShip IP")
    
    args = parser.parse_args()
    
    rover = Rover(args.rover_id, args.host, args.mothership)
    await rover.start()

if __name__ == "__main__":
    asyncio.run(main())