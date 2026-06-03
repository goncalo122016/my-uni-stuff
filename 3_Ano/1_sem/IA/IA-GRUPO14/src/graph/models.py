"""
Modelos de dados para o grafo da cidade (Node e Edge)
"""

from typing import Optional

try:
    from models.vehicle import Position
except ImportError:
    try:
        from .models.vehicle import Position
    except ImportError:
        # Definir Position localmente se não conseguir importar
        from dataclasses import dataclass
        @dataclass
        class Position:
            latitude: float
            longitude: float


class Node:
    """Nó do grafo representando uma localização"""
    
    def __init__(self, node_id: int, name: str, position: Position, 
                 node_type: str = "location", demand_level: str = "medium"):
        self.id = node_id
        self.name = name
        self.position = position
        self.type = node_type  # "location", "charging_station", "refuel_station"
        self.demand_level = demand_level  # "low", "medium", "high"
    
    def distance_to(self, other: 'Node') -> float:
        """Calcula distância até outro nó"""
        return self.position.distance_to(other.position)
    
    def __eq__(self, other):
        if isinstance(other, Node):
            return self.id == other.id
        return False
    
    def __hash__(self):
        return hash(self.id)
    
    def __repr__(self):
        return f"Node({self.id}, {self.name})"


class Edge:
    """Aresta do grafo representando uma ligação entre nós"""
    
    def __init__(self, from_node: int, to_node: int, distance_km: float, 
                 time_minutes: float, traffic_multiplier: float = 1.0):
        self.from_node = from_node
        self.to_node = to_node
        self.distance_km = distance_km
        self.time_minutes = time_minutes
        self.traffic_multiplier = traffic_multiplier  # Fator de trânsito
        self.is_blocked = False
    
    def get_effective_distance(self) -> float:
        """Retorna distância efetiva considerando trânsito"""
        return self.distance_km * self.traffic_multiplier
    
    def get_effective_time(self) -> float:
        """Retorna tempo efetivo considerando trânsito"""
        return self.time_minutes * self.traffic_multiplier
    
    def __repr__(self):
        return f"Edge({self.from_node}->{self.to_node}, {self.distance_km}km)"
