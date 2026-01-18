"""
Módulo para gerenciamento de clientes e suas preferências
"""

from dataclasses import dataclass
from typing import Optional
import random


@dataclass
class CustomerPreferences:
    """Preferências do cliente"""
    max_wait_time_minutes: float  # Tempo máximo de espera
    environmental_preference: str  # "electric", "any", "low_cost"
    priority: int = 1  # 1=normal, 2=priority, 3=urgent
    
    def prefers_electric(self) -> bool:
        """Retorna True se o cliente prefere veículo elétrico"""
        return self.environmental_preference == "electric"
    
    def accepts_any_vehicle(self) -> bool:
        """Retorna True se o cliente aceita qualquer tipo de veículo"""
        return self.environmental_preference == "any"


@dataclass
class Customer:
    """Representa um cliente com localização e preferências"""
    id: str
    pickup_location: str  # node_id
    dropoff_location: str  # node_id
    preferences: CustomerPreferences
    request_time: float  # Timestamp quando fez o pedido
    assigned_vehicle_id: Optional[str] = None
    pickup_time: Optional[float] = None
    dropoff_time: Optional[float] = None
    
    def get_wait_time(self, current_time: float) -> float:
        """Retorna tempo de espera em minutos"""
        if self.pickup_time:
            return (self.pickup_time - self.request_time) / 60.0
        return (current_time - self.request_time) / 60.0
    
    def get_total_trip_time(self) -> Optional[float]:
        """Retorna tempo total da viagem em minutos"""
        if self.dropoff_time and self.pickup_time:
            return (self.dropoff_time - self.pickup_time) / 60.0
        return None
    
    def is_completed(self) -> bool:
        """Retorna True se o pedido foi completado"""
        return self.dropoff_time is not None


def generate_random_customer(customer_id: str, available_nodes: list, request_time: float) -> Customer:
    """
    Gera um cliente aleatório com preferências aleatórias
    
    Args:
        customer_id: ID único do cliente
        available_nodes: Lista de node_ids disponíveis
        request_time: Timestamp do pedido
    
    Returns:
        Customer object
    """
    if len(available_nodes) < 2:
        raise ValueError("Precisa de pelo menos 2 nós para criar pickup e dropoff")
    
    # Selecionar locais aleatórios (diferentes)
    pickup = random.choice(available_nodes)
    dropoff = random.choice([n for n in available_nodes if n != pickup])
    
    # Gerar preferências aleatórias
    env_prefs = ["electric", "any", "low_cost"]
    env_weights = [0.3, 0.5, 0.2]  # 30% elétrico, 50% qualquer, 20% baixo custo
    
    preferences = CustomerPreferences(
        max_wait_time_minutes=random.uniform(5.0, 15.0),  # 5-15 minutos
        environmental_preference=random.choices(env_prefs, weights=env_weights)[0],
        priority=random.choices([1, 2, 3], weights=[0.7, 0.2, 0.1])[0]  # 70% normal, 20% priority, 10% urgent
    )
    
    return Customer(
        id=customer_id,
        pickup_location=pickup,
        dropoff_location=dropoff,
        preferences=preferences,
        request_time=request_time
    )
