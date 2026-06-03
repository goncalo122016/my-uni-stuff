"""
Funções utilitárias para cálculos e análises
"""

from typing import List, Dict
from models import Vehicle, TransportRequest
import math


def calculate_distance(lat1: float, lon1: float, lat2: float, lon2: float) -> float:
    """
    Calcula distância entre dois pontos usando fórmula de Haversine
    """
    R = 6371  # Raio da Terra em km
    lat1_rad, lon1_rad = math.radians(lat1), math.radians(lon1)
    lat2_rad, lon2_rad = math.radians(lat2), math.radians(lon2)
    
    dlat = lat2_rad - lat1_rad
    dlon = lon2_rad - lon1_rad
    
    a = math.sin(dlat/2)**2 + math.cos(lat1_rad) * math.cos(lat2_rad) * math.sin(dlon/2)**2
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a))
    
    return R * c


def calculate_co2_emissions(distance_km: float, vehicle_type: str) -> float:
    """
    Calcula emissões de CO2
    
    :param distance_km: Distância percorrida
    :param vehicle_type: Tipo de veículo
    :return: Emissões em kg
    """
    
    if vehicle_type == "electric":
        # Considera carga da rede elétrica
        return distance_km * 0.05  # kg CO2/km (valor aproximado)
    elif vehicle_type == "combustion":
        return distance_km * 0.21  # kg CO2/km
    
    return 0


def calculate_cost_per_km(vehicle: Vehicle) -> float:
    """
    Calcula custo operacional por km
    """
    return vehicle.operational_cost_per_km


def analyze_fleet_efficiency(vehicles: List[Vehicle]) -> Dict:
    """
    Analisa eficiência geral da frota
    """
    
    if not vehicles:
        return {}
    
    total_distance = sum(v.total_distance_km for v in vehicles)
    total_empty_distance = sum(v.empty_distance_km for v in vehicles)
    total_cost = sum(v.total_cost for v in vehicles)
    total_passengers = sum(v.current_passengers for v in vehicles)
    total_capacity = sum(v.passenger_capacity for v in vehicles)
    
    electric_vehicles = [v for v in vehicles if v.type.value == "electric"]
    combustion_vehicles = [v for v in vehicles if v.type.value == "combustion"]
    
    electric_co2 = sum(
        calculate_co2_emissions(v.total_distance_km, "electric")
        for v in electric_vehicles
    )
    combustion_co2 = sum(
        calculate_co2_emissions(v.total_distance_km, "combustion")
        for v in combustion_vehicles
    )
    
    return {
        'total_vehicles': len(vehicles),
        'electric_vehicles': len(electric_vehicles),
        'combustion_vehicles': len(combustion_vehicles),
        'total_distance_km': total_distance,
        'empty_distance_km': total_empty_distance,
        'loaded_distance_km': total_distance - total_empty_distance,
        'empty_distance_percentage': (total_empty_distance / total_distance * 100) if total_distance > 0 else 0,
        'total_cost_euros': total_cost,
        'cost_per_km': total_cost / total_distance if total_distance > 0 else 0,
        'average_occupancy_rate': (total_passengers / total_capacity * 100) if total_capacity > 0 else 0,
        'total_co2_emissions_kg': electric_co2 + combustion_co2,
        'electric_co2_kg': electric_co2,
        'combustion_co2_kg': combustion_co2
    }


def analyze_request_efficiency(requests: List[TransportRequest]) -> Dict:
    """
    Analisa eficiência na execução de pedidos
    """
    
    if not requests:
        return {}
    
    completed = [r for r in requests if r.status.value == "completed"]
    rejected = [r for r in requests if r.status.value == "rejected"]
    
    response_times = []
    for request in completed:
        if request.pickup_time and request.requested_time:
            response_time = (request.pickup_time - request.requested_time).total_seconds() / 60
            response_times.append(response_time)
    
    high_priority = [r for r in completed if r.priority.value == 2]
    low_priority = [r for r in completed if r.priority.value == 0]
    
    return {
        'total_requests': len(requests),
        'completed_requests': len(completed),
        'rejected_requests': len(rejected),
        'completion_rate': (len(completed) / len(requests) * 100) if requests else 0,
        'rejection_rate': (len(rejected) / len(requests) * 100) if requests else 0,
        'average_response_time_minutes': sum(response_times) / len(response_times) if response_times else 0,
        'max_response_time_minutes': max(response_times) if response_times else 0,
        'min_response_time_minutes': min(response_times) if response_times else 0,
        'high_priority_completed': len(high_priority),
        'low_priority_completed': len(low_priority)
    }


def compare_algorithms(results: Dict[str, Dict]) -> Dict:
    """
    Compara desempenho de diferentes algoritmos
    """
    
    comparison = {}
    for algo_name, metrics in results.items():
        comparison[algo_name] = {
            'average_response_time': metrics.get('average_response_time_minutes', 0),
            'completion_rate': metrics.get('completion_rate', 0),
            'efficiency_score': (
                (metrics.get('completion_rate', 0) * 0.4) +
                ((100 - metrics.get('average_response_time_minutes', 100)) * 0.3) +
                ((100 - metrics.get('rejection_rate', 100)) * 0.3)
            )
        }
    
    # Ordenar por efficiency_score
    sorted_results = sorted(
        comparison.items(),
        key=lambda x: x[1]['efficiency_score'],
        reverse=True
    )
    
    return {'ranking': sorted_results, 'details': comparison}


def generate_report(state) -> Dict:
    """
    Gera relatório completo da simulação
    """
    
    fleet_analysis = analyze_fleet_efficiency(state.vehicles)
    
    all_requests = state.metrics.get('total_requests', 0)
    completed = state.metrics.get('completed_requests', 0)
    rejected = state.metrics.get('rejected_requests', 0)
    
    requests_analysis = {
        'total_requests': all_requests,
        'completed_requests': completed,
        'rejected_requests': rejected,
        'completion_rate': (completed / all_requests * 100) if all_requests > 0 else 0,
        'rejection_rate': (rejected / all_requests * 100) if all_requests > 0 else 0,
        'average_response_time': state.metrics.get('average_response_time', 0),
        'average_occupancy_rate': state.metrics.get('average_occupancy_rate', 0)
    }
    
    return {
        'fleet_analysis': fleet_analysis,
        'requests_analysis': requests_analysis,
        'metrics': state.metrics
    }
