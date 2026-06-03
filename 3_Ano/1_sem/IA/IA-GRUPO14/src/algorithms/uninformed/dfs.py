"""
DFS (Depth-First Search) - Procura não informada em profundidade
"""

from typing import List, Tuple, Optional
from models import Vehicle, TransportRequest


def dfs_allocate_request(fleet: List[Vehicle], request: TransportRequest,
                         graph, start_node_id: int, max_depth: int = 10) -> Tuple[Optional[int], Optional[List[int]], float]:
    """
    DFS para alocar um pedido à melhor frota disponível
    
    :param fleet: Lista de veículos disponíveis
    :param request: Pedido de transporte
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param max_depth: Profundidade máxima
    :return: (vehicle_id, path, cost) ou (None, None, inf)
    """
    
    best_vehicle = None
    best_path = None
    best_cost = float('inf')
    
    for vehicle in fleet:
        if not vehicle.is_available():
            continue
        
        # DFS recursivo
        visited = set()
        
        def dfs_recursive(node_id: int, path: List[int], cost: float, depth: int) -> Tuple[Optional[List[int]], float]:
            nonlocal best_cost, best_path, best_vehicle
            
            if depth > max_depth or cost > vehicle.max_autonomy:
                return None, float('inf')
            
            if node_id in visited:
                return None, float('inf')
            
            visited.add(node_id)
            
            # Verificar se é um destino válido
            if node_id != start_node_id and cost < best_cost:
                best_path = path
                best_cost = cost
                best_vehicle = vehicle.id
            
            # Explorar vizinhos
            for neighbor in graph.get_neighbors(node_id):
                if neighbor not in visited:
                    edge_cost = graph.get_edge_distance(node_id, neighbor)
                    new_cost = cost + edge_cost
                    result_path, result_cost = dfs_recursive(
                        neighbor,
                        path + [neighbor],
                        new_cost,
                        depth + 1
                    )
            
            visited.remove(node_id)
            return best_path, best_cost
        
        dfs_recursive(start_node_id, [start_node_id], 0.0, 0)
    
    return best_vehicle, best_path, best_cost


def dfs_find_charging_point(graph, start_node_id: int, vehicle_type: str,
                           max_depth: int = 15) -> Optional[List[int]]:
    """
    DFS para encontrar ponto de recarga/abastecimento
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param vehicle_type: Tipo de veículo
    :param max_depth: Profundidade máxima de procura
    :return: Caminho para o ponto de recarga ou None
    """
    
    visited = set()
    
    def dfs_recursive(node_id: int, path: List[int], depth: int) -> Optional[List[int]]:
        if depth > max_depth:
            return None
        
        if node_id in visited:
            return None
        
        visited.add(node_id)
        
        # Verificar se é estação apropriada
        node = graph.get_node_by_id(node_id)
        if node:
            if vehicle_type == "electric" and node.type == "charging_station":
                return path
            elif vehicle_type == "combustion" and node.type == "refuel_station":
                return path
        
        # Explorar vizinhos
        for neighbor in graph.get_neighbors(node_id):
            if neighbor not in visited:
                result = dfs_recursive(neighbor, path + [neighbor], depth + 1)
                if result:
                    return result
        
        visited.remove(node_id)
        return None
    
    return dfs_recursive(start_node_id, [start_node_id], 0)


def dfs_explore_path_options(graph, start_node_id: int, 
                            max_paths: int = 5, max_depth: int = 8) -> List[List[int]]:
    """
    DFS para explorar múltiplas opções de caminhos
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param max_paths: Número máximo de caminhos a retornar
    :param max_depth: Profundidade máxima
    :return: Lista de caminhos
    """
    
    paths = []
    visited = set()
    
    def dfs_recursive(node_id: int, path: List[int], depth: int):
        if len(paths) >= max_paths or depth > max_depth:
            return
        
        if node_id in visited:
            return
        
        visited.add(node_id)
        
        if len(path) > 1:
            paths.append(path[:])
        
        # Explorar vizinhos
        for neighbor in graph.get_neighbors(node_id):
            if neighbor not in visited:
                dfs_recursive(neighbor, path + [neighbor], depth + 1)
        
        visited.remove(node_id)
    
    dfs_recursive(start_node_id, [start_node_id], 0)
    return paths
