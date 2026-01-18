"""
BFS (Breadth-First Search) - Procura não informada em largura
"""

from typing import List, Tuple, Optional
from collections import deque
from models import Vehicle, TransportRequest


class BFSSearchState:
    """Estado para procura BFS"""
    
    def __init__(self, vehicle: Vehicle, destination: int, visited_nodes: set = None,
                 path: List[int] = None, cost: float = 0.0):
        self.vehicle = vehicle
        self.destination = destination
        self.visited_nodes = visited_nodes or set()
        self.path = path or []
        self.cost = cost
    
    def __repr__(self):
        return f"BFSState(vehicle={self.vehicle.id}, dest={self.destination}, cost={self.cost})"


def bfs_allocate_request(fleet: List[Vehicle], request: TransportRequest, 
                         graph, start_node_id: int) -> Tuple[Optional[int], Optional[List[int]], float]:
    """
    BFS para alocar um pedido à melhor frota disponível
    
    :param fleet: Lista de veículos disponíveis
    :param request: Pedido de transporte
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :return: (vehicle_id, path, cost) ou (None, None, inf)
    """
    
    best_vehicle = None
    best_path = None
    best_cost = float('inf')
    
    for vehicle in fleet:
        if not vehicle.is_available() or not vehicle.can_reach_destination(
            graph.get_distance_between_nodes(start_node_id, 0)):
            continue
        
        # Procura BFS
        visited = set()
        queue = deque([(start_node_id, [start_node_id], 0.0)])
        
        while queue:
            current_node, path, cost = queue.popleft()
            
            if current_node in visited:
                continue
            visited.add(current_node)
            
            # Verificar se é o destino
            if current_node == 0:  # Nó destino simplificado
                if cost < best_cost and vehicle.can_reach_destination(cost):
                    best_vehicle = vehicle.id
                    best_path = path
                    best_cost = cost
                break
            
            # Expandir vizinhos
            for neighbor in graph.get_neighbors(current_node):
                if neighbor not in visited:
                    edge_cost = graph.get_edge_distance(current_node, neighbor)
                    new_cost = cost + edge_cost
                    
                    if new_cost <= vehicle.max_autonomy:
                        queue.append((neighbor, path + [neighbor], new_cost))
    
    return best_vehicle, best_path, best_cost


def bfs_find_nearest_station(graph, start_node_id: int, station_type: str) -> Optional[List[int]]:
    """
    BFS para encontrar a estação mais próxima
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param station_type: Tipo de estação ("charging_station" ou "refuel_station")
    :return: Caminho para a estação ou None
    """
    
    visited = set()
    queue = deque([(start_node_id, [start_node_id])])
    
    while queue:
        current_node, path = queue.popleft()
        
        if current_node in visited:
            continue
        visited.add(current_node)
        
        node = graph.get_node_by_id(current_node)
        if node and node.type == station_type:
            return path
        
        # Expandir vizinhos
        for neighbor in graph.get_neighbors(current_node):
            if neighbor not in visited:
                queue.append((neighbor, path + [neighbor]))
    
    return None
