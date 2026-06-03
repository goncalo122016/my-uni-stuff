"""
Heurísticas para algoritmos de procura informada
"""

from models import Vehicle, TransportRequest
from typing import Tuple


class TaxiHeuristics:
    """Conjunto de heurísticas para otimização de frota de táxis"""
    
    @staticmethod
    def autonomy_heuristic(vehicle: Vehicle, distance_to_goal: float) -> float:
        """
        Heurística de autonomia: prioriza veículos com autonomia suficiente
        Retorna penalidade se autonomia é insuficiente
        """
        if vehicle.current_autonomy < distance_to_goal:
            return float('inf')  # Impossível alcançar
        
        # Retorna a porcentagem de autonomia restante
        remaining_ratio = vehicle.current_autonomy / vehicle.max_autonomy
        return (1 - remaining_ratio) * 100
    
    @staticmethod
    def cost_heuristic(vehicle: Vehicle, distance_to_goal: float) -> float:
        """
        Heurística de custo operacional
        """
        return distance_to_goal * vehicle.operational_cost_per_km
    
    @staticmethod
    def time_heuristic(vehicle: Vehicle, distance_to_goal: float) -> float:
        """
        Heurística de tempo: estima tempo até o destino
        """
        if vehicle.type.value == "electric":
            avg_speed = 55  # km/h
        else:
            avg_speed = 60  # km/h
        
        estimated_time = (distance_to_goal / avg_speed) * 60  # em minutos
        return estimated_time
    
    @staticmethod
    def environmental_heuristic(vehicle: Vehicle, request: TransportRequest) -> float:
        """
        Heurística ambiental: considera preferência do cliente
        """
        if request.environmental_preference.value == 0:  # NO_PREFERENCE
            return 0
        elif request.environmental_preference.value == 1:  # PREFER_ELECTRIC
            if vehicle.type.value == "electric":
                return -10  # Bónus negativo (melhor)
            return 5
        elif request.environmental_preference.value == 2:  # REQUIRE_ELECTRIC
            if vehicle.type.value != "electric":
                return float('inf')
            return -20
        
        return 0
    
    @staticmethod
    def capacity_heuristic(vehicle: Vehicle, request: TransportRequest) -> float:
        """
        Heurística de capacidade: verifica espaço disponível
        """
        available_capacity = vehicle.passenger_capacity - vehicle.current_passengers
        if available_capacity < request.num_passengers:
            return float('inf')  # Não cabe
        
        # Retorna bónus baseado em capacidade disponível
        return 0
    
    @staticmethod
    def manhattan_distance_heuristic(current_pos: Tuple[float, float],
                                    goal_pos: Tuple[float, float]) -> float:
        """
        Heurística de Manhattan (distância de bloco)
        """
        return abs(current_pos[0] - goal_pos[0]) + abs(current_pos[1] - goal_pos[1])
    
    @staticmethod
    def euclidean_distance_heuristic(current_pos: Tuple[float, float],
                                     goal_pos: Tuple[float, float]) -> float:
        """
        Heurística Euclidiana (linha reta)
        """
        import math
        dx = current_pos[0] - goal_pos[0]
        dy = current_pos[1] - goal_pos[1]
        return math.sqrt(dx*dx + dy*dy)
    
    @staticmethod
    def combined_allocation_heuristic(vehicle: Vehicle, request: TransportRequest,
                                     distance_to_goal: float,
                                     time_weight: float = 0.3,
                                     cost_weight: float = 0.3,
                                     eco_weight: float = 0.2,
                                     autonomy_weight: float = 0.2) -> float:
        """
        Heurística combinada para alocação de pedidos
        Combina múltiplos critérios com pesos
        """
        
        # Calcular componentes
        time_score = TaxiHeuristics.time_heuristic(vehicle, distance_to_goal)
        cost_score = TaxiHeuristics.cost_heuristic(vehicle, distance_to_goal)
        autonomy_score = TaxiHeuristics.autonomy_heuristic(vehicle, distance_to_goal)
        eco_score = TaxiHeuristics.environmental_heuristic(vehicle, request)
        
        # Verificar impossibilidades
        if autonomy_score == float('inf') or eco_score == float('inf'):
            return float('inf')
        
        # Normalizar scores
        max_time = 60  # minutos
        max_cost = 50  # euros
        max_autonomy_cost = 100
        
        time_normalized = min(time_score / max_time, 1.0)
        cost_normalized = min(cost_score / max_cost, 1.0)
        autonomy_normalized = min(autonomy_score / max_autonomy_cost, 1.0)
        eco_normalized = eco_score if eco_score >= 0 else -eco_score / 100
        
        # Combinar com pesos
        total_score = (
            time_weight * time_normalized +
            cost_weight * cost_normalized +
            autonomy_weight * autonomy_normalized +
            eco_weight * eco_normalized
        )
        
        return total_score
