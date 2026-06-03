import threading
import time
from typing import Dict, Any, Optional
from jeepney.low_level import Boolean
from common.message_types import RoverState

class RoverStateManager:
    def __init__(self, rover_id: str):
        self.rover_id = rover_id
        self._lock = threading.Lock()
        
        # Estado inicial
        self._state = {
            'rover_state': RoverState.IDLE.value,
            'position': {'x': 0.0, 'y': 0.0, 'z': 0.0},
            'speed': {'x': 0.0, 'y': 0.0, 'z': 0.0},
            'temperature': 20.0,
            'battery': 100.0,
            'tire_pressure_psi': 32.0,
            'current_mission_id': None,
            'mission_progress': 0.0,
            'last_update': time.time()
        }

        self._charging_point = {"x": 50.0, "y": 50.0, "z": 0.0}
        self._mission_info: Optional[Dict[str, Any]] = None
        self.charging_target = None
    
    def get_state(self) -> Dict[str, Any]:
        """Obtém estado atual (thread-safe)"""
        with self._lock:
            return self._state.copy()
    
    def set_state(self, rover_state: RoverState):
        """Define estado do rover"""
        with self._lock:
            self._state['rover_state'] = rover_state.value
            self._state['last_update'] = time.time()
    
    def update_position(self, position: Dict[str, float]):
        """Atualiza posição"""
        with self._lock:
            self._state['position'] = position.copy()
            self._state['last_update'] = time.time()
    
    def update_speed(self, speed: Dict[str, float]):
        """Atualiza velocidade"""
        with self._lock:
            self._state['speed'] = speed.copy()
            self._state['last_update'] = time.time()

    def update_battery(self, battery: float, clamp: bool = True):
        """Atualiza bateria

        clamp=True  -> limita a [0,100] (comportamento normal)
        clamp=False -> aceita qualquer valor (para testes)
        """
        with self._lock:
            value = max(0.0, min(100.0, battery)) if clamp else battery

            self._state['battery'] = value
            self._state['last_update'] = time.time()

            # Verificar estado crítico (continua igual)
            if value <= 5.0:
                self._state['rover_state'] = RoverState.ERROR.value
            elif value <= 20.0:
                self._state['rover_state'] = RoverState.CHARGING.value

    def update_temperature(self, temperature: float):
        """Atualiza temperatura"""
        with self._lock:
            self._state['temperature'] = temperature
            self._state['last_update'] = time.time()

    def update_tire_pressure(self, pressure_psi: float):
        """Atualiza a pressão dos pneus em psi."""
        with self._lock:
            self._state['tire_pressure_psi'] = pressure_psi
            self._state['last_update'] = time.time()

    
    def set_mission(self, mission_id: str, mission_info: Dict[str, Any]):
        """Define nova missão"""
        with self._lock:
            self._state['current_mission_id'] = mission_id
            self._state['mission_progress'] = 0.0
            self._state['rover_state'] = RoverState.IN_MISSION.value
            self._state['last_update'] = time.time()
            self._mission_info = mission_info.copy()
    
    def update_progress(self, progress: float):
        """Atualiza progresso da missão"""
        with self._lock:
            self._state['mission_progress'] = max(0.0, min(1.0, progress))
            self._state['last_update'] = time.time()
    
    def complete_mission(self):
        """Completa missão atual"""
        with self._lock:
            self._state['current_mission_id'] = None
            self._state['mission_progress'] = 0.0
            self._state['rover_state'] = RoverState.IDLE.value
            self._state['last_update'] = time.time()
            self._mission_info = None
    
    def has_active_mission(self) -> bool:
        """Verifica se tem missão ativa"""
        with self._lock:
            return self._state['current_mission_id'] is not None
    
    def get_mission_info(self) -> Optional[Dict[str, Any]]:
        """Obtém informação da missão atual"""
        with self._lock:
            return self._mission_info.copy() if self._mission_info else None

    def get_is_charging(self) -> bool:
        with self._lock:
            return self._state['rover_state'] == RoverState.CHARGING.value

    def update_is_charging(self, is_charging: bool):
        with self._lock:
            self._state['rover_state'] = (
                RoverState.CHARGING.value if is_charging else RoverState.IN_MISSION.value
            )
            self._state['last_update'] = time.time()

