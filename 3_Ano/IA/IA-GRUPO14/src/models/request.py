from enum import Enum
from dataclasses import dataclass
from datetime import datetime
from typing import Optional

try:
    from .vehicle import Position
except ImportError:
    from models.vehicle import Position


class RequestStatus(Enum):
    """Estados de um pedido de transporte"""
    PENDING = "pending"
    ASSIGNED = "assigned"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    REJECTED = "rejected"
    CANCELLED = "cancelled"


class RequestPriority(Enum):
    """Níveis de prioridade"""
    LOW = 0
    NORMAL = 1
    URGENT = 2


class EnvironmentalPreference(Enum):
    """Preferência ambiental do cliente"""
    NO_PREFERENCE = 0
    PREFER_ELECTRIC = 1
    REQUIRE_ELECTRIC = 2


@dataclass
class TransportRequest:
    """Classe representando um pedido de transporte"""
    
    request_id: int
    origin: Position
    destination: Position
    num_passengers: int
    requested_time: datetime
    priority: RequestPriority
    environmental_preference: EnvironmentalPreference = EnvironmentalPreference.NO_PREFERENCE
    max_wait_time_minutes: int = 15
    special_requirements: str = ""
    
    # Atributos de operação
    status: RequestStatus = RequestStatus.PENDING
    assigned_vehicle_id: Optional[int] = None
    pickup_time: Optional[datetime] = None
    dropoff_time: Optional[datetime] = None
    actual_distance_km: float = 0.0
    actual_cost: float = 0.0
    customer_rating: Optional[float] = None
    
    def is_expired(self, current_time: datetime) -> bool:
        """Verifica se o pedido expirou (tempo de espera máximo atingido)"""
        wait_time = (current_time - self.requested_time).total_seconds() / 60
        return wait_time > self.max_wait_time_minutes
    
    def get_wait_time_minutes(self, current_time: datetime) -> float:
        """Retorna tempo de espera atual em minutos"""
        return (current_time - self.requested_time).total_seconds() / 60
    
    def get_info(self) -> dict:
        """Retorna informações do pedido"""
        return {
            "id": self.request_id,
            "passengers": self.num_passengers,
            "priority": self.priority.name,
            "status": self.status.value,
            "assigned_vehicle": self.assigned_vehicle_id,
            "environmental_pref": self.environmental_preference.name,
            "requested_time": self.requested_time.strftime("%H:%M:%S") if self.requested_time else "N/A"
        }
    
    def __repr__(self):
        return (f"Request({self.request_id}, {self.num_passengers} pax, "
                f"{self.priority.name}, {self.status.value})")


class Station:
    """Classe representando uma estação de recarga/abastecimento"""
    
    def __init__(self, station_id: str, name: str, position: Position,
                 station_type: str, available_slots: int, 
                 service_time_minutes: int, priority: int = 0):
        self.id = station_id
        self.name = name
        self.position = position
        self.type = station_type  # "charging_station" ou "refuel_station"
        self.max_slots = available_slots
        self.available_slots = available_slots
        self.service_time_minutes = service_time_minutes
        self.priority = priority
        self.queue = []  # Veículos em fila
        self.is_operational = True
        
    def is_available(self) -> bool:
        """Verifica se há slots disponíveis"""
        return self.available_slots > 0 and self.is_operational
    
    def can_serve_vehicle(self, vehicle_type: str) -> bool:
        """Verifica se a estação pode servir este tipo de veículo"""
        if self.type == "charging_station":
            return vehicle_type == "electric"
        elif self.type == "refuel_station":
            return vehicle_type == "combustion"
        return False
    
    def add_vehicle_to_queue(self, vehicle_id: int):
        """Adiciona veículo à fila"""
        if self.is_available():
            self.available_slots -= 1
            self.queue.append(vehicle_id)
            return True
        return False
    
    def remove_vehicle_from_queue(self, vehicle_id: int):
        """Remove veículo da fila"""
        if vehicle_id in self.queue:
            self.queue.remove(vehicle_id)
            self.available_slots += 1
            return True
        return False
    
    def simulate_failure(self):
        """Simula falha da estação"""
        self.is_operational = False
    
    def restore_service(self):
        """Restaura serviço da estação"""
        self.is_operational = True
    
    def get_info(self) -> dict:
        """Retorna informações da estação"""
        return {
            "id": self.id,
            "name": self.name,
            "type": self.type,
            "available_slots": f"{self.available_slots}/{self.max_slots}",
            "queue_length": len(self.queue),
            "operational": "Yes" if self.is_operational else "No",
            "service_time": f"{self.service_time_minutes}min"
        }
    
    def __repr__(self):
        return f"Station({self.name}, available={self.available_slots}, queue={len(self.queue)})"
