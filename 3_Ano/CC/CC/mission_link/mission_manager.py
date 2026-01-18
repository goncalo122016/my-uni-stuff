import asyncio
import logging
from typing import Dict, Optional, Callable
from enum import Enum
from dataclasses import dataclass, field
import time

from common.message_types import MissionMessage, MissionMessageType, Area, Position

class MissionStatus(Enum):
    PENDING = "pending"
    ASSIGNED = "assigned"
    ACTIVE = "active"
    COMPLETED = "completed"
    ABORTED = "aborted"
    CANCELLED = "cancelled"
    PAUSED = "paused"

@dataclass
class Mission:
    mission_id: str
    rover_id: Optional[str]
    area: Area
    tasks: str
    duration: int
    progress_period: int
    status: MissionStatus = MissionStatus.PENDING
    progress: float = 0.0
    start_time: Optional[float] = None
    end_time: Optional[float] = None
    created_time: float = field(default_factory=time.time)

class MissionManager:
    def __init__(self):
        self.missions: Dict[str, Mission] = {}
        self.rover_missions: Dict[str, str] = {}  # rover_id -> mission_id
        self.mission_counter = 0
        self.logger = logging.getLogger(__name__)

        self.protocol = None
        
        # Callbacks
        self.on_mission_assigned: Optional[Callable] = None
        self.on_mission_progress: Optional[Callable] = None
        self.on_mission_completed: Optional[Callable] = None
        self.on_mission_aborted: Optional[Callable] = None
        self.on_mission_paused: Optional[Callable] = None

    def set_protocol(self, protocol):
        """Permite injetar o MissionLinkProtocol vindo da MotherShip."""
        self.protocol = protocol
    
    def create_mission(self, area: Area, tasks: str, duration: int, progress_period: int = 120) -> str:
        self.mission_counter += 1
        mission_id = f"M-{self.mission_counter:03d}"
    
        mission = Mission(
            mission_id=mission_id,
            rover_id=None,
            area=area,
            tasks=tasks,
            duration=duration,
            progress_period=progress_period
        )
    
        self.missions[mission_id] = mission
    
        if self.protocol:
            try:
                self.protocol.try_assign_waiting_rover(mission)
            except Exception as e:
                self.logger.error(f"[ML] Erro ao tentar atribuir missão criada automaticamente: {e}")
    
        return mission_id
    
    def assign_mission(self, mission_id: str, rover_id: str) -> bool:
        if mission_id not in self.missions:
            return False
        
        if self.protocol and rover_id in self.protocol.waiting_rovers:
            self.protocol.waiting_rovers.pop(rover_id, None)

        if rover_id in self.rover_missions:
            self.logger.warning(f"[ML] Rover {rover_id} já tem missão ativa")
            return False

        mission = self.missions[mission_id]
        if mission.status != MissionStatus.PENDING:
            return False

        mission.rover_id = rover_id
        mission.status = MissionStatus.ASSIGNED
        self.rover_missions[rover_id] = mission_id

        # Callback (logging, UI, API…)
        if self.on_mission_assigned:
            self.on_mission_assigned(mission)

        # Enviar ao rover via UDP MissionLink
        if self.protocol:
            try:
                self.protocol.send_mission_assign(mission, rover_id)
            except Exception as e:
                self.logger.error(f"[ML] Erro ao enviar assign ao rover {rover_id}: {e}")

        return True
    
    def start_mission(self, mission_id: str) -> bool:
        """Inicia missão"""
        if mission_id not in self.missions:
            return False
        
        mission = self.missions[mission_id]
        if mission.status != MissionStatus.ASSIGNED:
            return False
        
        mission.status = MissionStatus.ACTIVE
        mission.start_time = time.time()
        
        self.logger.info(f"[ML] Missão {mission_id} iniciada")
        return True
    
    def update_progress(self, mission_id: str, progress: float, position: Position, battery: int) -> bool:
        """Atualiza progresso da missão"""
        if mission_id not in self.missions:
            return False
        
        mission = self.missions[mission_id]
        if mission.status != MissionStatus.ACTIVE:
            return False
        
        mission.progress = progress
        
        if self.on_mission_progress:
            self.on_mission_progress(mission, position, battery)
        
        return True
    
    def complete_mission(self, mission_id: str) -> bool:
        """Completa missão"""
        if mission_id not in self.missions:
            return False
        
        mission = self.missions[mission_id]
        if mission.status != MissionStatus.ACTIVE:
            return False
        
        mission.status = MissionStatus.COMPLETED
        mission.end_time = time.time()
        mission.progress = 1.0
        
        # Remover rover da missão ativa
        if mission.rover_id and mission.rover_id in self.rover_missions:
            del self.rover_missions[mission.rover_id]
        
        if self.on_mission_completed:
            self.on_mission_completed(mission)
        
        return True
    
    def abort_mission(self, mission_id: str) -> bool:
        if mission_id not in self.missions:
            return False
        
        mission = self.missions[mission_id]
        
        # Permitir aborto de qualquer estado não final
        if mission.status in [MissionStatus.COMPLETED, MissionStatus.CANCELLED, MissionStatus.ABORTED]:
            self.logger.warning(f"[ML] Missão {mission_id} já está finalizada ({mission.status.value})")
            return False
        
        mission.status = MissionStatus.ABORTED
        mission.end_time = time.time()
        
        if mission.rover_id and mission.rover_id in self.rover_missions:
            del self.rover_missions[mission.rover_id]
        
        if self.on_mission_aborted:
            self.on_mission_aborted(mission)
        
        self.logger.info(f"[ML] Missão {mission_id} abortada com sucesso")
        return True

    def cancel_mission(self, mission_id: str) -> bool:
        if mission_id not in self.missions:
            return False

        mission = self.missions[mission_id]
        old_status = mission.status

        mission.status = MissionStatus.CANCELLED
        mission.end_time = time.time()

        if mission.rover_id and mission.rover_id in self.rover_missions:
            del self.rover_missions[mission.rover_id]

        # Enviar cancelamento
        if self.protocol and mission.rover_id:
            try:
                self.protocol.send_cancel_mission(mission_id, mission.rover_id)
            except Exception as e:
                self.logger.error(f"[ML] Erro ao enviar cancel ao rover {mission.rover_id}: {e}")

        self.logger.info(f"[ML] Missão {mission_id} cancelada (era {old_status.value})")
        return True
    
    def get_mission(self, mission_id: str) -> Optional[Mission]:
        """Obtém missão por ID"""
        return self.missions.get(mission_id)
    
    def get_rover_mission(self, rover_id: str) -> Optional[Mission]:
        """Obtém missão ativa do rover"""
        mission_id = self.rover_missions.get(rover_id)
        if mission_id:
            return self.missions.get(mission_id)
        return None
    
    def get_all_missions(self) -> Dict[str, Mission]:
        """Obtém todas as missões"""
        return self.missions.copy()
    
    def get_active_missions(self) -> Dict[str, Mission]:
        """Obtém missões ativas"""
        return {
            mid: mission for mid, mission in self.missions.items()
            if mission.status == MissionStatus.ACTIVE
        }

    def get_pending_mission(self) -> Optional[Mission]:
        """Retorna a próxima missão PENDING (FIFO) ou None se não houver."""
        pending = [
            m for m in sorted(self.missions.values(), key=lambda x: x.created_time)
            if m.status == MissionStatus.PENDING
        ]
        return pending[0] if pending else None

    def pause_mission(self, mission_id: str, is_charging: bool) -> bool:
        """
        Se is_charging == True e missão estiver ACTIVE -> fica PAUSED.
        Se is_charging == False e missão estiver PAUSED -> volta a ACTIVE.
        """
        if mission_id not in self.missions:
            return False

        mission = self.missions[mission_id]

        # Rover começou a carregar -> pausa missão
        if is_charging and mission.status == MissionStatus.ACTIVE:
            mission.status = MissionStatus.PAUSED
            self.logger.info(f"[ML] Missão {mission_id} pausada (rover a carregar)")
            return True

        # Rover terminou de carregar -> retoma missão
        if not is_charging and mission.status == MissionStatus.PAUSED:
            mission.status = MissionStatus.ACTIVE
            self.logger.info(f"[ML] Missão {mission_id} retomada após carregamento")
            return True

        return False
