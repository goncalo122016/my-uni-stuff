"""
IDS (Iterative Deepening Depth-First Search) - Procura iterativa em profundidade
"""

from typing import List, Tuple, Optional
from models import Vehicle


def ids_find_path(graph, start_node_id: int, end_node_id: int,
                 max_depth: int = 15) -> Optional[List[int]]:
    """
    IDS para encontrar um caminho entre dois nós
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :param max_depth: Profundidade máxima
    :return: Caminho ou None
    """
    
    for depth in range(1, max_depth + 1):
        result = dfs_limited(graph, start_node_id, end_node_id, depth)
        if result:
            return result
    
    return None


def dfs_limited(graph, start_node_id: int, end_node_id: int,
               limit_depth: int) -> Optional[List[int]]:
    """
    DFS com limite de profundidade
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :param limit_depth: Limite de profundidade
    :return: Caminho ou None
    """
    
    def dfs_recursive(node_id: int, path: List[int], visited: set, depth: int) -> Optional[List[int]]:
        if depth > limit_depth:
            return None
        
        if node_id == end_node_id:
            return path
        
        for neighbor in graph.get_neighbors(node_id):
            if neighbor not in visited:
                edge = graph.get_edge(node_id, neighbor)
                if edge and not edge.is_blocked:
                    visited.add(neighbor)
                    result = dfs_recursive(neighbor, path + [neighbor], visited, depth + 1)
                    visited.remove(neighbor)
                    
                    if result:
                        return result
        
        return None
    
    visited = {start_node_id}
    return dfs_recursive(start_node_id, [start_node_id], visited, 1)


def ids_allocate_request(fleet: List[Vehicle], request, graph,
                        start_node_id: int, end_node_id: int,
                        max_depth: int = 10) -> Tuple[Optional[int], Optional[List[int]]]:
    """
    IDS para alocar pedido a melhor veículo
    
    :param fleet: Lista de veículos
    :param request: Pedido de transporte
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :param max_depth: Profundidade máxima
    :return: (vehicle_id, path) ou (None, None)
    """
    
    path = ids_find_path(graph, start_node_id, end_node_id, max_depth)
    
    if not path:
        return None, None
    
    # Encontrar veículo que pode fazer esta rota
    for vehicle in fleet:
        if vehicle.is_available():
            distance = sum(
                graph.get_edge_distance(path[i], path[i+1])
                for i in range(len(path)-1)
            )
            if vehicle.can_reach_destination(distance):
                return vehicle.id, path
    
    return None, None


def ids_find_nearest_station(graph, start_node_id: int, station_type: str,
                            max_depth: int = 10) -> Optional[List[int]]:
    """
    IDS para encontrar estação de recarga/abastecimento
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param station_type: Tipo de estação
    :param max_depth: Profundidade máxima
    :return: Caminho para estação ou None
    """
    
    for depth in range(1, max_depth + 1):
        result = dfs_limited_station(graph, start_node_id, station_type, depth)
        if result:
            return result
    
    return None


def dfs_limited_station(graph, start_node_id: int, station_type: str,
                       limit_depth: int) -> Optional[List[int]]:
    """
    DFS com limite para encontrar estação
    """
    
    def dfs_recursive(node_id: int, path: List[int], visited: set, depth: int) -> Optional[List[int]]:
        if depth > limit_depth:
            return None
        
        node = graph.get_node_by_id(node_id)
        if node and node.type == station_type:
            return path
        
        for neighbor in graph.get_neighbors(node_id):
            if neighbor not in visited:
                edge = graph.get_edge(node_id, neighbor)
                if edge and not edge.is_blocked:
                    visited.add(neighbor)
                    result = dfs_recursive(neighbor, path + [neighbor], visited, depth + 1)
                    visited.remove(neighbor)
                    
                    if result:
                        return result
        
        return None
    
    visited = {start_node_id}
    return dfs_recursive(start_node_id, [start_node_id], visited, 1)
