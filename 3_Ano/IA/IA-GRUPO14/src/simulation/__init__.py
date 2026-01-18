"""
Motor de simulação dinâmica
"""

import random
from datetime import datetime, timedelta
from typing import List, Tuple, Optional

try:
    from models import Vehicle, TransportRequest, RequestStatus, RequestPriority, EnvironmentalPreference, VehicleStatus
    from algorithms.informed import a_star_allocate_request
    from algorithms.informed import greedy_allocate_request
except ImportError:
    try:
        from ..models import Vehicle, TransportRequest, RequestStatus, RequestPriority, EnvironmentalPreference, VehicleStatus
        from ..algorithms.informed import a_star_allocate_request
        from ..algorithms.informed import greedy_allocate_request
    except ImportError:
        from src.models import Vehicle, TransportRequest, RequestStatus, RequestPriority, EnvironmentalPreference, VehicleStatus
        from src.algorithms.informed import a_star_allocate_request
        from src.algorithms.informed import greedy_allocate_request


class SimulationEngine:
    """Motor para simulação dinâmica da frota de táxis"""
    
    def __init__(self, state):
        self.state = state
        self.current_time = datetime.now()
        self.simulation_speed = 1.0  # Multiplicador de tempo
        self.algorithm = "a_star"  # Algoritmo de alocação padrão
        self.active_requests = []
        self.completed_requests = []
        self.rejected_requests = []
        self.traffic_conditions = {}  # node_id -> multiplier
        
    def generate_random_request(self) -> TransportRequest:
        """Gera um pedido de transporte aleatório"""
        
        # Selecionar nós aleatórios
        nodes = list(self.state.graph.nodes.values())
        if len(nodes) < 2:
            return None
        
        origin_node = random.choice(nodes)
        destination_node = random.choice([n for n in nodes if n.id != origin_node.id])
        
        request = TransportRequest(
            request_id=self.state.metrics['total_requests'] + 1,
            origin=origin_node.position,
            destination=destination_node.position,
            num_passengers=random.randint(1, 4),
            requested_time=self.current_time,
            priority=random.choice(list(RequestPriority)),
            environmental_preference=random.choice(list(EnvironmentalPreference)),
            max_wait_time_minutes=random.randint(5, 20),
            special_requirements=""
        )
        
        self.state.metrics['total_requests'] += 1
        return request
    
    def allocate_request(self, request: TransportRequest) -> bool:
        """
        Aloca um pedido de transporte a um veículo
        
        :param request: Pedido a ser alocado
        :return: True se alocado com sucesso
        """
        
        # Filtrar veículos disponíveis
        available_vehicles = [v for v in self.state.vehicles if v.is_available()]
        if not available_vehicles:
            self.rejected_requests.append(request)
            self.state.metrics['rejected_requests'] += 1
            request.status = RequestStatus.REJECTED
            return False
        
        # Encontrar nó de origem e destino mais próximo
        start_node_id = self._find_nearest_node(request.origin)
        end_node_id = self._find_nearest_node(request.destination)
        
        if start_node_id is None or end_node_id is None:
            self.rejected_requests.append(request)
            self.state.metrics['rejected_requests'] += 1
            return False
        
        # Usar algoritmo de alocação
        if self.algorithm == "a_star":
            vehicle_id, path, cost = a_star_allocate_request(
                available_vehicles, request, self.state.graph,
                start_node_id, end_node_id
            )
        else:  # greedy
            vehicle_id, path, cost = greedy_allocate_request(
                available_vehicles, request, self.state.graph,
                start_node_id, end_node_id
            )
        
        if vehicle_id is None:
            self.rejected_requests.append(request)
            self.state.metrics['rejected_requests'] += 1
            request.status = RequestStatus.REJECTED
            return False
        
        # Alocar veículo ao pedido
        vehicle = next((v for v in self.state.vehicles if v.id == vehicle_id), None)
        if vehicle:
            vehicle.status = VehicleStatus.BUSY
            vehicle.current_request_id = request.request_id
            vehicle.current_passengers = request.num_passengers
            
            request.status = RequestStatus.ASSIGNED
            request.assigned_vehicle_id = vehicle_id
            request.pickup_time = self.current_time
            
            self.active_requests.append(request)
            self.state.metrics['vehicles_in_service'] += 1
            
            # Simular viagem
            if path:
                self._simulate_trip(vehicle, request, path)
            
            return True
        
        return False
    
    def _find_nearest_node(self, position) -> Optional[int]:
        """Encontra nó mais próximo de uma posição"""
        nearest_node_id = None
        min_distance = float('inf')
        
        for node in self.state.graph.nodes.values():
            distance = position.distance_to(node.position)
            if distance < min_distance:
                min_distance = distance
                nearest_node_id = node.id
        
        return nearest_node_id
    
    def _simulate_trip(self, vehicle: Vehicle, request: TransportRequest, path: List[int]):
        """Simula uma viagem do veículo"""
        
        # Calcular distância total
        total_distance = 0
        for i in range(len(path) - 1):
            total_distance += self.state.graph.get_edge_distance(path[i], path[i+1])
        
        # Atualizar veículo
        vehicle.travel(total_distance, with_passengers=True)
        
        # Atualizar pedido
        request.status = RequestStatus.COMPLETED
        request.dropoff_time = self.current_time + timedelta(
            minutes=(total_distance / 60) * 60  # Aproximado
        )
        request.actual_distance_km = total_distance
        request.actual_cost = total_distance * vehicle.operational_cost_per_km
        
        # Atualizar métricas
        self.state.metrics['total_distance'] += total_distance
        self.state.metrics['total_cost'] += request.actual_cost
        self.state.metrics['completed_requests'] += 1
        
        # Calcular CO2 (táxis a combustão)
        if vehicle.type.value == "combustion":
            co2_per_km = 0.21  # kg CO2/km
            self.state.metrics['total_co2_emissions'] += total_distance * co2_per_km
        
        # Liberar veículo
        vehicle.status = VehicleStatus.IDLE
        vehicle.current_passengers = 0
        vehicle.current_request_id = None
        self.state.metrics['vehicles_in_service'] -= 1
        
        if request in self.active_requests:
            self.active_requests.remove(request)
        self.completed_requests.append(request)
    
    def check_vehicle_autonomy(self):
        """Verifica autonomia dos veículos e marca para recarga/abastecimento"""
        
        for vehicle in self.state.vehicles:
            if vehicle.type.value == "electric":
                threshold = vehicle.max_autonomy * 0.2  # 20%
                if vehicle.current_autonomy < threshold:
                    vehicle.status = VehicleStatus.RECHARGING
                    self.state.metrics['vehicles_charging'] += 1
            else:
                threshold = vehicle.max_autonomy * 0.15  # 15%
                if vehicle.current_autonomy < threshold:
                    vehicle.status = VehicleStatus.REFUELING
                    self.state.metrics['vehicles_refueling'] += 1
    
    def simulate_charging(self, vehicle: Vehicle, charge_time_minutes: int = 30):
        """Simula recarga de veículo elétrico"""
        
        if vehicle.type.value == "electric":
            vehicle.current_autonomy = vehicle.max_autonomy
            vehicle.status = VehicleStatus.IDLE
            self.state.metrics['vehicles_charging'] -= 1
    
    def simulate_refueling(self, vehicle: Vehicle, refuel_time_minutes: int = 5):
        """Simula reabastecimento de veículo a combustão"""
        
        if vehicle.type.value == "combustion":
            vehicle.current_autonomy = vehicle.max_autonomy
            vehicle.status = VehicleStatus.IDLE
            self.state.metrics['vehicles_refueling'] -= 1
    
    def update_metrics(self):
        """Atualiza métricas da simulação"""
        
        # Tempo médio de resposta
        if self.completed_requests:
            response_times = [
                (r.pickup_time - r.requested_time).total_seconds() / 60
                for r in self.completed_requests
                if r.pickup_time and r.requested_time
            ]
            if response_times:
                self.state.metrics['average_response_time'] = sum(response_times) / len(response_times)
        
        # Taxa de ocupação média
        if self.state.vehicles:
            total_passengers = sum(v.current_passengers for v in self.state.vehicles)
            total_capacity = sum(v.passenger_capacity for v in self.state.vehicles)
            self.state.metrics['average_occupancy_rate'] = (total_passengers / total_capacity) * 100 if total_capacity > 0 else 0
    
    def get_simulation_status(self) -> dict:
        """Retorna status atual da simulação"""
        
        self.update_metrics()
        
        return {
            'current_time': self.current_time.isoformat(),
            'active_requests': len(self.active_requests),
            'completed_requests': self.state.metrics['completed_requests'],
            'rejected_requests': self.state.metrics['rejected_requests'],
            'metrics': self.state.metrics,
            'vehicles': [
                {
                    'id': v.id,
                    'name': v.name,
                    'status': v.status.value,
                    'autonomy': f"{v.current_autonomy:.1f}/{v.max_autonomy}",
                    'passengers': f"{v.current_passengers}/{v.passenger_capacity}",
                    'position': [v.position.longitude, v.position.latitude]
                }
                for v in self.state.vehicles
            ]
        }
    
    def step(self, time_step_minutes: int = 1):
        """Executa um passo da simulação"""
        
        self.current_time += timedelta(minutes=time_step_minutes)
        
        # Gerar novos pedidos (probabilidade)
        if random.random() < 0.3:  # 30% de chance
            request = self.generate_random_request()
            if request:
                self.allocate_request(request)
        
        # Verificar autonomia
        self.check_vehicle_autonomy()
        
        # Atualizar métricas
        self.update_metrics()
