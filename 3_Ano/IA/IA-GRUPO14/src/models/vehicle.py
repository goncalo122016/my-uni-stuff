from enum import Enum
from dataclasses import dataclass
from typing import List, Dict


class VehicleType(Enum):
    """Tipos de veículos disponíveis"""
    COMBUSTION = "combustion"
    ELECTRIC = "electric"


class VehicleStatus(Enum):
    """Estados possíveis de um veículo"""
    IDLE = "idle"
    BUSY = "busy"
    RECHARGING = "recharging"
    REFUELING = "refueling"
    MAINTENANCE = "maintenance"


class FuelType(Enum):
    """Tipos de combustível"""
    GASOLINE = "gasoline"
    DIESEL = "diesel"
    ELECTRICITY = "electricity"


@dataclass
class Position:
    """Representação de uma posição geográfica"""
    longitude: float
    latitude: float
    
    def distance_to(self, other: 'Position') -> float:
        """Calcula distância aproximada em km usando fórmula de Haversine"""
        import math
        
        R = 6371  # Raio da Terra em km
        lat1, lon1 = math.radians(self.latitude), math.radians(self.longitude)
        lat2, lon2 = math.radians(other.latitude), math.radians(other.longitude)
        
        dlat = lat2 - lat1
        dlon = lon2 - lon1
        
        a = math.sin(dlat/2)**2 + math.cos(lat1) * math.cos(lat2) * math.sin(dlon/2)**2
        c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
        
        return R * c


class Vehicle:
    """Classe representando um veículo de táxi"""
    
    def __init__(self, vehicle_id: int, name: str, vehicle_type: VehicleType, 
                 fuel_type: FuelType, position: Position, max_autonomy: float,
                 current_autonomy: float, passenger_capacity: int,
                 operational_cost_per_km: float, status: VehicleStatus = VehicleStatus.IDLE,
                 eco_score: int = 5):
        self.id = vehicle_id
        self.name = name
        self.type = vehicle_type
        self.fuel_type = fuel_type
        self.position = position
        self.max_autonomy = max_autonomy
        self.current_autonomy = current_autonomy
        self.passenger_capacity = passenger_capacity
        self.operational_cost_per_km = operational_cost_per_km
        self.status = status
        self.eco_score = eco_score  # 0-10, onde 10 é mais ecológico
        
        # Atributos de operação
        self.current_passengers = 0
        self.total_distance_km = 0
        self.empty_distance_km = 0
        self.current_request_id = None
        self.service_history = []
        self.total_cost = 0
    
    def is_available(self) -> bool:
        """Verifica se o veículo está disponível para aceitar novos pedidos"""
        return self.status == VehicleStatus.IDLE and self.current_autonomy > 0
    
    def needs_refuel(self) -> bool:
        """Verifica se o veículo precisa reabastecer (menos de 10% de combustível)"""
        return self.current_autonomy < (self.max_autonomy * 0.10)
    
    def fuel_percentage(self) -> float:
        """Retorna a porcentagem de combustível atual"""
        return (self.current_autonomy / self.max_autonomy) * 100.0
    
    def can_reach_destination(self, distance_km: float) -> bool:
        """Verifica se o veículo tem autonomia suficiente"""
        return self.current_autonomy >= distance_km
    
    def travel(self, distance_km: float, with_passengers: bool = True):
        """Simula uma viagem e consome combustível baseado na distância"""
        # Consumir combustível proporcional à distância
        self.current_autonomy -= distance_km
        if self.current_autonomy < 0:
            self.current_autonomy = 0
        
        self.total_distance_km += distance_km
        if not with_passengers:
            self.empty_distance_km += distance_km
        self.total_cost += distance_km * self.operational_cost_per_km
    
    def refuel(self):
        """Reabastece o veículo até 90% da capacidade máxima"""
        self.current_autonomy = self.max_autonomy * 0.90
        self.status = VehicleStatus.IDLE
    
    def start_refueling(self):
        """Inicia o processo de reabastecimento"""
        if self.fuel_type == FuelType.ELECTRICITY:
            self.status = VehicleStatus.RECHARGING
        else:
            self.status = VehicleStatus.REFUELING
    
    def get_info(self) -> Dict:
        """Retorna informações do veículo"""
        return {
            "id": self.id,
            "name": self.name,
            "type": self.type.value,
            "status": self.status.value,
            "autonomy": f"{self.current_autonomy:.1f}/{self.max_autonomy}",
            "passengers": f"{self.current_passengers}/{self.passenger_capacity}",
            "total_distance": f"{self.total_distance_km:.1f} km",
            "empty_distance": f"{self.empty_distance_km:.1f} km",
            "total_cost": f"€{self.total_cost:.2f}",
            "eco_score": f"{self.eco_score}/10"
        }
    
    def __repr__(self):
        return f"Vehicle({self.name}, {self.status.value}, autonomy={self.current_autonomy:.1f}km)"
