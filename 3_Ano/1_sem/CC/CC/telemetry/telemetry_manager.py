import asyncio
import logging
import time
from typing import Dict, Optional, List, Callable
from dataclasses import dataclass, field
from common.message_types import TelemetryMessage, RoverState, Position, Speed

@dataclass
class RoverTelemetry:
    rover_id: str
    last_telemetry: Optional[TelemetryMessage] = None
    connection_time: float = field(default_factory=time.time)
    last_update: float = field(default_factory=time.time)
    is_connected: bool = True

class TelemetryManager:
    def __init__(self):
        self.rovers: Dict[str, RoverTelemetry] = {}
        self.telemetry_history: Dict[str, List[TelemetryMessage]] = {}
        self.max_history_size = 100
        self.logger = logging.getLogger(__name__)
        
        # Callbacks para eventos
        self.on_rover_connected: Optional[Callable] = None
        self.on_rover_disconnected: Optional[Callable] = None
        self.on_telemetry_received: Optional[Callable] = None
        self.on_low_battery: Optional[Callable] = None
        self.on_state_change: Optional[Callable] = None
        self.on_high_temperature: Optional[Callable] = None
    
    def add_rover(self, rover_id: str) -> RoverTelemetry:
        rover_telemetry = RoverTelemetry(rover_id=rover_id)
        self.rovers[rover_id] = rover_telemetry
        self.telemetry_history[rover_id] = []
        
        if self.on_rover_connected:
            self.on_rover_connected(rover_id)
        
        return rover_telemetry
    
    def remove_rover(self, rover_id: str) -> bool:
        if rover_id in self.rovers:
            self.rovers[rover_id].is_connected = False
            
            if self.on_rover_disconnected:
                self.on_rover_disconnected(rover_id)
            
            return True
        return False
    
    def update_telemetry(self, telemetry: TelemetryMessage) -> bool:
        """Atualiza telemetria do rover"""
        rover_id = telemetry.rover_id
        
        # Verificar se rover existe
        if rover_id not in self.rovers:
            self.add_rover(rover_id)
        
        rover_telemetry = self.rovers[rover_id]
        old_state = rover_telemetry.last_telemetry.state if rover_telemetry.last_telemetry else None
        
        # Atualizar telemetria
        rover_telemetry.last_telemetry = telemetry
        rover_telemetry.last_update = time.time()
        rover_telemetry.is_connected = True
        
        # Adicionar ao histórico
        history = self.telemetry_history[rover_id]
        history.append(telemetry)
        
        # Limitar tamanho do histórico
        if len(history) > self.max_history_size:
            history.pop(0)
        
        # Verificar eventos
        self._check_events(telemetry, old_state)
        
        if self.on_telemetry_received:
            self.on_telemetry_received(telemetry)
        
        return True
    
    def _check_events(self, telemetry: TelemetryMessage, old_state: Optional[RoverState]):
        """Verifica eventos especiais"""
        # Bateria baixa
        if telemetry.battery <= 20 and self.on_low_battery:
            self.on_low_battery(telemetry.rover_id, telemetry.battery)

        # temperatura alta
        if telemetry.temperature >= 100.0 and self.on_high_temperature:
            self.on_high_temperature(telemetry.rover_id, telemetry.temperature)
        
        # Mudança de estado
        if old_state and old_state != telemetry.state and self.on_state_change:
            self.on_state_change(telemetry.rover_id, old_state, telemetry.state)
    
    def get_rover_telemetry(self, rover_id: str) -> Optional[TelemetryMessage]:
        """Obtém última telemetria do rover"""
        rover = self.rovers.get(rover_id)
        return rover.last_telemetry if rover else None
    
    def get_all_rovers(self) -> Dict[str, TelemetryMessage]:
        """Obtém telemetria de todos os rovers"""
        result = {}
        for rover_id, rover in self.rovers.items():
            if rover.last_telemetry and rover.is_connected:
                result[rover_id] = rover.last_telemetry
        return result
    
    def get_rover_history(self, rover_id: str, limit: Optional[int] = None) -> List[TelemetryMessage]:
        """Obtém histórico de telemetria do rover"""
        history = self.telemetry_history.get(rover_id, [])
        if limit:
            return history[-limit:]
        return history.copy()
    
    def cleanup_disconnected_rovers(self, timeout: float = 30.0):
        """Remove rovers desconectados há muito tempo"""
        current_time = time.time()
        to_remove = []
        
        for rover_id, rover in self.rovers.items():
            if current_time - rover.last_update > timeout:
                to_remove.append(rover_id)
        
        for rover_id in to_remove:
            self.remove_rover(rover_id)

    def get_all_rover_histories(self, limit: Optional[int] = None):
        """
        Devolve o histórico de telemetria de TODOS os rovers.
        Formato:
        {
            "R-001": [TelemetryMessage, ...],
            "R-002": [...],
            ...
        }
        """
        result = {}
        for rover_id, history in self.telemetry_history.items():
            if limit:
                result[rover_id] = history[-limit:]
            else:
                result[rover_id] = history.copy()
        return result