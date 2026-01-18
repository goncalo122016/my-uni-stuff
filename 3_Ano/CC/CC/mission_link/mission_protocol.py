import asyncio
import logging
from typing import Optional, Callable, Dict
from common.message_types import MissionMessage, MissionMessageType, Area, Position
from common.protocol_config import ProtocolConfig
from .reliability import ReliabilityManager
from .mission_manager import Mission, MissionManager


class MissionLinkProtocol(asyncio.DatagramProtocol):
    def __init__(self, mission_manager: MissionManager, is_mothership: bool = False, telemetry_manager=None):
        self.mission_manager = mission_manager
        self.is_mothership = is_mothership
        self.telemetry_manager = telemetry_manager
        self.reliability = ReliabilityManager()
        self.transport = None
        self.logger = logging.getLogger(__name__)
        self.rover_addresses: Dict[str, tuple] = {}
        self.waiting_rovers: Dict[str, tuple] = {}

        if self.is_mothership and self.mission_manager is not None:
            self.mission_manager.on_mission_assigned = self._on_mission_assigned_callback

        self.message_handlers: Dict[MissionMessageType, Callable] = {
            MissionMessageType.MISSION_HELLO: self._handle_mission_hello,
            MissionMessageType.MISSION_REQUEST: self._handle_mission_request,
            MissionMessageType.MISSION_ASSIGN: self._handle_mission_assign,
            MissionMessageType.MISSION_ACKNOWLEDGMENT: self._handle_mission_ack,
            MissionMessageType.MISSION_PROGRESS: self._handle_mission_progress,
            MissionMessageType.MISSION_COMPLETE: self._handle_mission_complete,
            MissionMessageType.MISSION_ABORT: self._handle_mission_abort,
            MissionMessageType.MISSION_CANCEL: self._handle_mission_cancel,
            MissionMessageType.MISSION_PAUSED: self._handle_mission_paused,
        }

    # CONNECTION MANAGEMENT
    def connection_made(self, transport):
        self.transport = transport
        self.reliability.set_transport(transport)
        self.logger.info("[ML] Mission Link Protocol iniciado")

    def error_received(self, exc):
        self.logger.error(f"[ML] [CONNECTION_ERROR] {exc}")

    def _register_rover_address(self, rover_id: str, addr: tuple):
        """Regista o endereço (IP, porto) de um rover ativo"""
        if rover_id:
            self.rover_addresses[rover_id] = addr
            self.logger.info(f"[ML] [ROVER_REG] {rover_id} registrado em {addr}")

    # RECEÇÃO DE PACOTES
    def datagram_received(self, data, addr):
        try:
            message = MissionMessage.from_bytes(data)

            if message.ack is not None:
                self.reliability.handle_ack(message.ack)
                self.logger.info(f"[ML] [ACK_RX: {message.ack}] Recebido ACK de {addr} para seq {message.ack}")

            handler = self.message_handlers.get(message.type)
            if handler:
                handler(message, addr)
            else:
                self.logger.warning(f"[UNKNOWN_MSG] Tipo de mensagem não suportado: {message.type}")
        except Exception as e:
            self.logger.error(f"[DECODE_ERROR] Erro ao processar mensagem de {addr}: {e}")

    # MOTHERSHIP CALLBACKS
    def _on_mission_assigned_callback(self, mission):
        """Callback chamado quando MissionManager.assign_mission() é executado"""
        rover_id = mission.rover_id
        if not rover_id:
            return

        addr = self.waiting_rovers.pop(rover_id, None)
        if not addr:
            self.logger.info(f"[ML] [ASSIGN_PENDING] Missão {mission.mission_id} atribuída a {rover_id}, mas rover não está em espera.")
            return

        assign_message = MissionMessage(
            type=MissionMessageType.MISSION_ASSIGN,
            seq=self.reliability.get_next_seq(),
            mission_id=mission.mission_id,
            area=mission.area,
            tasks=mission.tasks,
            duration=mission.duration,
            progress_period=mission.progress_period
        )

        try:
            self.send_message(assign_message, addr)
        except Exception as e:
            self.logger.error(f"[ASSIGN_ERROR] Falha ao enviar MISSION_ASSIGN para {rover_id} em {addr}: {e}")

    # HANDLERS DE MENSAGENS
    def _handle_mission_hello(self, message: MissionMessage, addr):
        """Rover -> MotherShip: Handshake inicial para registar endereço"""
        if not self.is_mothership:
            return

        rover_id = message.rover_id
        if not rover_id:
            self.logger.warning(f"[HELLO_RX] Mensagem HELLO sem rover_id de {addr}")
            return

        self.rover_addresses[rover_id] = addr
        self.logger.info(f"[ML] [HELLO_RX] Rover {rover_id} registado no endereço {addr}")

        self.send_ack(message.seq, addr, rover_id=rover_id)

    def _handle_mission_request(self, message: MissionMessage, addr):
        if not self.is_mothership:
            return

        rover_id = message.rover_id
        if not rover_id:
            self.logger.warning(f"[REQUEST_RX] Pedido sem rover_id de {addr}")
            return

        self.logger.info(f"[ML] [REQUEST_RX] Pedido de missão recebido de {addr}: rover_id={rover_id}")

        pending = self.mission_manager.get_pending_mission()
        if pending:
            self.mission_manager.assign_mission(pending.mission_id, rover_id)

        self.waiting_rovers[rover_id] = addr
        # Send ACK mesmo que a missão não esteja disponível
        self.send_ack(message.seq, addr, rover_id=rover_id)
        self.logger.info(f"[ML] [REQUEST_WAIT] Rover {rover_id} aguardará nova missão.")

    def _handle_mission_assign(self, message: MissionMessage, addr):
        if self.is_mothership:
            return

        self.logger.info(
            f"[ML] [ASSIGN_RX] Nova missão recebida de {addr}: id={message.mission_id}, "
            f"duração={message.duration}s, tarefas={message.tasks}"
        )

        self.send_ack(message.seq, addr, mission_id=message.mission_id)

        if hasattr(self, 'on_mission_received'):
            self.on_mission_received(message)

    def _handle_mission_ack(self, message: MissionMessage, addr):
        if self.is_mothership and message.mission_id:
            self.mission_manager.start_mission(message.mission_id)

    def _handle_mission_progress(self, message: MissionMessage, addr):
        if not self.is_mothership:
            return
        if not message.mission_id:
            self.logger.warning(f"[PROGRESS_RX] Mensagem sem mission_id de {addr}")
            return
        temp_str = ""
        if self.telemetry_manager and message.rover_id:
            try:
                telem = self.telemetry_manager.get_rover_telemetry(message.rover_id)
                if telem:
                    temp_str = f", temperatura={telem.temperature:.1f}ºC"
            except Exception as e:
                self.logger.error(f"[ML] Erro ao obter temperatura para rover {message.rover_id}: {e}")


        self.logger.info(
            f"[ML] [PROGRESS_RX] Missão {message.mission_id} de {addr}: "
            f"progresso={message.progress * 100:.2f}%, posição=({message.position.x:.1f},"
            f"{message.position.y:.1f},{message.position.z:.1f}), bateria={message.battery:.1f}%"
            f"{temp_str}"
        )

        self.mission_manager.update_progress(
            message.mission_id,
            message.progress or 0.0,
            message.position or Position(0, 0, 0),
            message.battery or 0
        )

        battery = float(message.battery or 0)
        pos = message.position or Position(0, 0, 0)

        mission = self.mission_manager.get_mission(message.mission_id)
        if mission:
            area = mission.area
            target_x = (area.x1 + area.x2) / 2.0
            target_y = (area.y1 + area.y2) / 2.0
            target_z = 0.0

            dx = pos.x - target_x
            dy = pos.y - target_y
            dz = pos.z - target_z
            dist_center = (dx * dx + dy * dy + dz * dz) ** 0.5

            # Se já passou 50% de progresso e está junto ao centro -> está a executar tarefa
            if mission.progress >= 0.5 and mission.progress < 1.0 and dist_center < 1.0:
                self.logger.info(
                    f"[ML] [Tarefa] Rover {mission.rover_id} a executar a tarefa:{mission.tasks} na missão {mission.mission_id}"
                )

        if battery < 20.0:
            self.logger.info(
                f"[ML] [ROVER_STATUS] Bateria baixa ({battery:.1f}%), "
                "rover a dirigir-se para o posto de carregamento (50, 50, 0)..."
            )
            self.logger.info(
                "[ML] [ROVER_STATUS] A caminho do posto de carregamento: "
                f"pos=({pos.x:.1f},{pos.y:.1f},{pos.z:.1f}), "
                f"bateria={battery:.1f}%"
            )

            if abs(pos.x - 50.0) < 0.5 and abs(pos.y - 50.0) < 0.5:
                self.logger.info(
                    "[ML] [ROVER_STATUS] Rover no posto de carregamento (50, 50, 0)."
                )

        # Pausar/retomar missão conforme bateria
        self.mission_manager.pause_mission(message.mission_id, is_charging=(battery < 20.0))

        self.send_ack(message.seq, addr, mission_id=message.mission_id)

    def _handle_mission_complete(self, message: MissionMessage, addr):
        if self.is_mothership and message.mission_id:
            self.mission_manager.complete_mission(message.mission_id)
            self.send_ack(message.seq, addr, mission_id=message.mission_id)
            self.logger.info(f"[ML] [COMPLETE_RX] Missão {message.mission_id} completada por rover em {addr}")

    def _handle_mission_abort(self, message: MissionMessage, addr):
        if self.is_mothership and message.mission_id:
            self.mission_manager.abort_mission(message.mission_id)
            self.send_ack(message.seq, addr, mission_id=message.mission_id)
            self.logger.info(f"[ML] [ABORT_RX] Missão {message.mission_id} abortada por rover em {addr}")

    def _handle_mission_cancel(self, message: MissionMessage, addr):
        if self.is_mothership:
            return
        if hasattr(self, 'on_mission_cancelled'):
            self.on_mission_cancelled(message)
            self.send_ack(message.seq, addr, mission_id=message.mission_id)
            self.logger.info(f"[ML] [CANCEL_RX] Missão {message.mission_id} cancelada por rover em {addr}")

    # ENVIO DE MENSAGENS
    def send_hello(self, rover_id: str, mothership_addr: tuple):
        if self.is_mothership:
            self.logger.warning("[HELLO_TX] Ignorado — protocolo está em modo MotherShip.")
            return

        hello_msg = MissionMessage(
            type=MissionMessageType.MISSION_HELLO,
            seq=self.reliability.get_next_seq(),
            rover_id=rover_id
        )

        self.send_message(hello_msg, mothership_addr)
        self.logger.info(f"[ML] [HELLO_TX] HELLO enviado para MotherShip {mothership_addr}")

    def send_message(self, message: MissionMessage, addr):
        packet = message.to_bytes()

        if message.type == MissionMessageType.MISSION_ACKNOWLEDGMENT:
            # ACK NÃO É FIÁVEL
            self.transport.sendto(packet, addr)
            return

        seq = message.seq or self.reliability.next_seq()
        message.seq = seq  # garante seq válido
        packet = message.to_bytes()

        self.reliability.send_reliable(packet, addr, seq)

    def send_ack(self, ack_seq: int, addr: tuple, rover_id: Optional[str] = None, mission_id: Optional[str] = None):
        ack_message = MissionMessage(
            type=MissionMessageType.MISSION_ACKNOWLEDGMENT,
            seq=self.reliability.get_next_seq(),
            ack=ack_seq,
            rover_id=rover_id,
            mission_id=mission_id
        )
        self.send_message(ack_message, addr)

    def request_mission(self, rover_id: str, mothership_addr):
        message = MissionMessage(
            type=MissionMessageType.MISSION_REQUEST,
            seq=self.reliability.get_next_seq(),
            rover_id=rover_id
        )
        self.send_message(message, mothership_addr)

    def try_assign_waiting_rover(self, mission: Mission):
        """Tenta atribuir a missão a um rover que está à espera."""
        if not self.waiting_rovers:
            return False
    
        # FIFO -> primeiro rover da fila
        rover_id, addr = next(iter(self.waiting_rovers.items()))
        del self.waiting_rovers[rover_id]
    
        # Atribuir no MissionManager
        ok = self.mission_manager.assign_mission(mission.mission_id, rover_id)
        if ok:
            self.logger.info(f"[ML] [AUTO_ASSIGN] Missão {mission.mission_id} atribuída ao rover em espera {rover_id}")
            return True
    
        return False

    def send_progress(self, mission_id: str, progress: float, position: Position, battery: int, mothership_addr):
        message = MissionMessage(
            type=MissionMessageType.MISSION_PROGRESS,
            seq=self.reliability.get_next_seq(),
            mission_id=mission_id,
            rover_id=getattr(self, "rover_id", None),
            progress=progress,
            position=position,
            battery=battery
        )
        self.send_message(message, mothership_addr)

    def complete_mission(self, mission_id: str, mothership_addr):
        message = MissionMessage(
            type=MissionMessageType.MISSION_COMPLETE,
            seq=self.reliability.get_next_seq(),
            mission_id=mission_id
        )
        self.send_message(message, mothership_addr)
        self.logger.info(f"[ML] [COMPLETE_TX] Missão {mission_id} completada e enviada para MotherShip {mothership_addr}")

    def abort_mission(self, mission_id: str, mothership_addr):
        message = MissionMessage(
            type=MissionMessageType.MISSION_ABORT,
            seq=self.reliability.get_next_seq(),
            mission_id=mission_id
        )
        self.send_message(message, mothership_addr)
        self.logger.info(f"[ML] [ABORT_TX] Missão {mission_id} abortada e enviada para MotherShip {mothership_addr}")

    def send_mission_assign(self, mission: Mission, rover_id: str):
        assign_message = MissionMessage(
            type=MissionMessageType.MISSION_ASSIGN,
            seq=self.reliability.get_next_seq(),
            mission_id=mission.mission_id,
            area=mission.area,
            tasks=mission.tasks,
            duration=mission.duration,
            progress_period=mission.progress_period
        )

        rover_addr = self.rover_addresses.get(rover_id)
        if not rover_addr:
            self.logger.error(f"[ASSIGN_ERROR] Endereço do rover {rover_id} desconhecido. Não foi possível enviar missão.")
            return

        self.send_message(assign_message, rover_addr)
        self.logger.info(f"[ML] [ASSIGN_TX] Missão {mission.mission_id} enviada diretamente a {mission.rover_id} ({rover_addr})")

    def send_cancel_mission(self, mission_id: str, rover_id: str):
        rover_addr = self.rover_addresses.get(rover_id)
    
        if not rover_addr:
            self.logger.error(f"[CANCEL_ERROR] Endereço do rover {rover_id} desconhecido. Não foi possível enviar cancelamento.")
            return
    
        cancel_msg = MissionMessage(
            type=MissionMessageType.MISSION_CANCEL,
            seq=self.reliability.get_next_seq(),
            mission_id=mission_id,
            rover_id=rover_id
        )
    
        self.send_message(cancel_msg, rover_addr)
        self.logger.info(f"[ML] [CANCEL_TX] Cancelamento da missão {mission_id} enviado para {rover_id} ({rover_addr})")

    def _handle_mission_paused(self, message: MissionMessage, addr):
        # Do lado da MotherShip não faz sentido tratar esta mensagem
        if self.is_mothership:
            return

        self.logger.warning(f"[ML] [PAUSED_RX] Missão {message.mission_id} foi colocada em pausa pela MotherShip (de {addr})")

        # Callback no rover para parar movimento / lógica local
        if hasattr(self, "on_mission_paused") and self.on_mission_paused:
            try:
                self.on_mission_paused(message)
            except Exception as e:
                self.logger.error(f"[ML] Erro no callback on_mission_paused: {e}")
