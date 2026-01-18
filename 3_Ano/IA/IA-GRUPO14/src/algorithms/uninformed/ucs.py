"""
UCS (Uniform Cost Search) - Procura não informada por custo uniforme
"""

import heapq
from typing import List, Tuple, Optional
from models import Vehicle, TransportRequest


def ucs_find_cheapest_path(graph, start_node_id: int, end_node_id: int,
                          vehicle: Vehicle) -> Tuple[Optional[List[int]], float]:
    """
    UCS para encontrar o caminho mais barato (menor custo operacional)
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :param vehicle: Veículo em questão
    :return: (path, total_cost) ou (None, inf)
    """
    
    # Fila de prioridade: (custo_acumulado, nó_atual, caminho)
    heap = [(0.0, start_node_id, [start_node_id])]
    visited = {}  # node_id -> best_cost_to_reach
    
    while heap:
        current_cost, current_node, path = heapq.heappop(heap)
        
        # Se já visitamos com custo menor, pular
        if current_node in visited and visited[current_node] <= current_cost:
            continue
        
        visited[current_node] = current_cost
        
        # Se atingimos o destino
        if current_node == end_node_id:
            return path, current_cost
        
        # Se excedeu autonomia do veículo
        if current_cost > vehicle.max_autonomy:
            continue
        
        # Expandir vizinhos
        for neighbor in graph.get_neighbors(current_node):
            if neighbor not in visited or visited[neighbor] > current_cost:
                edge = graph.get_edge(current_node, neighbor)
                if edge and not edge.is_blocked:
                    # Custo da aresta é a distância
                    edge_cost = edge.distance_km * vehicle.operational_cost_per_km
                    new_cost = current_cost + edge_cost
                    new_path = path + [neighbor]
                    
                    if new_cost <= vehicle.max_autonomy:
                        heapq.heappush(heap, (new_cost, neighbor, new_path))
    
    return None, float('inf')


def ucs_allocate_request_min_cost(fleet: List[Vehicle], request: TransportRequest,
                                 graph, start_node_id: int, end_node_id: int) -> Tuple[Optional[int], Optional[List[int]], float]:
    """
    UCS para alocar pedido ao veículo com menor custo operacional
    
    :param fleet: Lista de veículos
    :param request: Pedido de transporte
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :return: (vehicle_id, path, cost) ou (None, None, inf)
    """
    
    best_vehicle = None
    best_path = None
    best_cost = float('inf')
    
    for vehicle in fleet:
        if not vehicle.is_available():
            continue
        
        path, cost = ucs_find_cheapest_path(graph, start_node_id, end_node_id, vehicle)
        
        if path and cost < best_cost:
            best_vehicle = vehicle.id
            best_path = path
            best_cost = cost
    
    return best_vehicle, best_path, best_cost


def ucs_find_nearest_by_time(graph, start_node_id: int, 
                            vehicle: Vehicle) -> Tuple[Optional[List[int]], float]:
    """
    UCS para encontrar próxima localização pelo tempo de percurso
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param vehicle: Veículo
    :return: (path, time_minutes)
    """
    
    # Fila de prioridade: (tempo_acumulado, nó_atual, caminho)
    heap = [(0.0, start_node_id, [start_node_id])]
    visited = {}
    
    best_destinations = []
    
    while heap and len(best_destinations) < 5:
        current_time, current_node, path = heapq.heappop(heap)
        
        if current_node in visited and visited[current_node] <= current_time:
            continue
        
        visited[current_node] = current_time
        
        if current_node != start_node_id and current_time > 0:
            best_destinations.append((path, current_time))
        
        # Expandir vizinhos
        for neighbor in graph.get_neighbors(current_node):
            if neighbor not in visited:
                edge_time = graph.get_edge_time(current_node, neighbor)
                new_time = current_time + edge_time
                
                # Limite de tempo: 30 minutos
                if new_time <= 30:
                    heapq.heappush(heap, (new_time, neighbor, path + [neighbor]))
    
    if best_destinations:
        return best_destinations[0]
    return None, float('inf')
