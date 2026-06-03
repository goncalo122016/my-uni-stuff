"""
Carregador de dataset JSON e inicializador do estado da simulação
"""

import json
try:
    from .models import Vehicle, VehicleType, FuelType, VehicleStatus, Position, Station
    from .graph import CityGraph
except ImportError:
    from models import Vehicle, VehicleType, FuelType, VehicleStatus, Position, Station
    from graph import CityGraph


class SimulationState:
    """Estado geral da simulação"""
    
    def __init__(self):
        self.graph = CityGraph()
        self.vehicles = []
        self.stations = []
        self.requests = []
        self.metrics = {}
        self.current_time = 0


def load_dataset(filepath: str) -> SimulationState:
    """
    Carrega dataset JSON e cria estado da simulação
    Suporta ambos os formatos: [lon, lat] e {"latitude": ..., "longitude": ...}
    
    :param filepath: Caminho para arquivo JSON
    :return: SimulationState inicializado
    """
    
    with open(filepath, 'r', encoding='utf-8') as f:
        data = json.load(f)
    
    state = SimulationState()
    
    # Helper para extrair posição (suporta ambos formatos)
    def get_position(pos_data):
        if isinstance(pos_data, dict):
            return Position(
                longitude=pos_data.get('longitude', pos_data.get('lon', 0)),
                latitude=pos_data.get('latitude', pos_data.get('lat', 0))
            )
        else:  # Array [longitude, latitude]
            return Position(longitude=pos_data[0], latitude=pos_data[1])
    
    # Carregar grafo - APENAS localizações normais (pontos de busca de passageiros)
    # Suportar tanto 'nodes' quanto 'graph_nodes' para compatibilidade
    nodes_data = data.get('nodes', data.get('graph_nodes', []))
    for node_data in nodes_data:
        pos = get_position(node_data['position'])
        state.graph.add_node(
            node_data['id'],
            node_data['name'],
            pos,
            node_data.get('type', 'location'),
            node_data.get('demand_level', 'medium')
        )
    
    # Carregar arestas entre localizações
    # Suportar tanto 'edges' quanto 'graph_edges' para compatibilidade
    edges_data = data.get('edges', data.get('graph_edges', []))
    for edge_data in edges_data:
        state.graph.add_edge(
            edge_data['from'],
            edge_data['to'],
            edge_data['distance_km'],
            edge_data['time_minutes'],
            bidirectional=True
        )
    
    # Carregar veículos
    for vehicle_data in data.get('vehicles', []):
        pos = get_position(vehicle_data['position'])
        
        vehicle_type = VehicleType.COMBUSTION if vehicle_data['type'] == 'combustion' else VehicleType.ELECTRIC
        fuel_type = FuelType[vehicle_data['fuel_type'].upper().replace(' ', '')]
        
        # Suportar nomes de campos diferentes
        max_autonomy = vehicle_data.get('autonomy', vehicle_data.get('max_autonomy', 500))
        current_autonomy = vehicle_data.get('current_fuel', vehicle_data.get('current_autonomy', max_autonomy))
        capacity = vehicle_data.get('capacity', vehicle_data.get('passenger_capacity', 4))
        cost = vehicle_data.get('cost_per_km', vehicle_data.get('operational_cost_per_km', 0.25))
        
        vehicle = Vehicle(
            vehicle_id=vehicle_data['id'],
            name=vehicle_data['name'],
            vehicle_type=vehicle_type,
            fuel_type=fuel_type,
            position=pos,
            max_autonomy=max_autonomy,
            current_autonomy=current_autonomy,
            passenger_capacity=capacity,
            operational_cost_per_km=cost,
            status=VehicleStatus.IDLE,
            eco_score=vehicle_data.get('eco_score', 5)
        )
        state.vehicles.append(vehicle)
    
    # Carregar estações
    for station_data in data.get('stations', []):
        pos = get_position(station_data['position'])
        
        # Suportar formatos diferentes de capacidade
        capacity = station_data.get('capacity', 
                                   station_data.get('available_chargers',
                                   station_data.get('available_pumps', 4)))
        
        # Suportar formatos diferentes de tempo
        service_time = station_data.get('charge_time_minutes',
                                       station_data.get('charging_time_minutes',
                                       station_data.get('refuel_time_minutes', 5)))
        
        station = Station(
            station_id=station_data['id'],
            name=station_data['name'],
            position=pos,
            station_type=station_data['type'],
            available_slots=capacity,
            service_time_minutes=service_time,
            priority=station_data.get('priority', 0)
        )
        state.stations.append(station)
    
    # Inicializar métricas
    state.metrics = {
        'total_requests': 0,
        'completed_requests': 0,
        'rejected_requests': 0,
        'average_response_time': 0.0,
        'total_distance': 0.0,
        'empty_distance': 0.0,
        'total_cost': 0.0,
        'total_co2_emissions': 0.0,
        'average_occupancy_rate': 0.0,
        'vehicles_in_service': 0,
        'vehicles_charging': 0,
        'vehicles_refueling': 0
    }
    
    return state


def save_state(state: SimulationState, filepath: str):
    """
    Salva estado da simulação em arquivo JSON
    """
    
    data = {
        'metrics': state.metrics,
        'current_time': state.current_time,
        'vehicles': [
            {
                'id': v.id,
                'name': v.name,
                'type': v.type.value,
                'status': v.status.value,
                'autonomy': v.current_autonomy,
                'passengers': v.current_passengers,
                'position': [v.position.longitude, v.position.latitude],
                'total_distance': v.total_distance_km,
                'total_cost': v.total_cost
            }
            for v in state.vehicles
        ]
    }
    
    with open(filepath, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
