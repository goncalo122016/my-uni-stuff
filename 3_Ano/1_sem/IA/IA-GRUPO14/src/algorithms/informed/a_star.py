"""
A* Search - Procura informada com heurística admissível
"""

import heapq
from typing import List, Tuple, Optional
from models import Vehicle, TransportRequest
from algorithms.informed.heuristics import TaxiHeuristics


def a_star_allocate_request(fleet: List[Vehicle], request: TransportRequest,
                           graph, start_node_id: int, end_node_id: int) -> Tuple[Optional[int], Optional[List[int]], float]:
    """
    A* para alocar pedido ao melhor veículo
    Usa heurística admissível (nunca subestima)
    
    :param fleet: Lista de veículos
    :param request: Pedido de transporte
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :return: (vehicle_id, path, cost)
    """
    
    best_vehicle = None
    best_path = None
    best_cost = float('inf')
    
    for vehicle in fleet:
        if not vehicle.is_available():
            continue
        
        path, cost = a_star_find_path(graph, start_node_id, end_node_id, vehicle, request)
        
        if path and cost < best_cost:
            best_vehicle = vehicle.id
            best_path = path
            best_cost = cost
    
    return best_vehicle, best_path, best_cost


def a_star_find_path(graph, start_node_id: int, end_node_id: int,
                    vehicle: Vehicle, request: TransportRequest = None, optimization_criterion: str = "distance") -> Tuple[Optional[List[int]], float]:
    """
    A* para encontrar caminho ótimo entre dois pontos
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :param vehicle: Veículo que irá fazer a rota
    :param request: Pedido associado (opcional)
    :param optimization_criterion: "distance" para otimizar distância, "time" para otimizar tempo
    :return: (path, cost)
    """
    
    # Heap: (f_score, g_score, node_id, path, distance_traveled)
    start_node = graph.get_node_by_id(start_node_id)
    end_node = graph.get_node_by_id(end_node_id)
    
    if not start_node or not end_node:
        return None, float('inf')
    
    # Heurística inicial
    h_start = TaxiHeuristics.euclidean_distance_heuristic(
        (start_node.position.longitude, start_node.position.latitude),
        (end_node.position.longitude, end_node.position.latitude)
    )
    
    heap = [(h_start, 0.0, start_node_id, [start_node_id], 0.0)]
    visited = {}
    g_score = {start_node_id: 0.0}
    
    while heap:
        f_score, g_current, current_node, path, distance = heapq.heappop(heap)
        
        # Se já visitamos com score menor, pular
        if current_node in visited:
            continue
        
        visited[current_node] = g_current
        
        # Se atingimos o destino
        if current_node == end_node_id:
            return path, g_current
        
        # Se excedeu autonomia
        if distance > vehicle.max_autonomy:
            continue
        
        # Expandir vizinhos
        for neighbor in graph.get_neighbors(current_node):
            if neighbor not in visited:
                edge = graph.get_edge(current_node, neighbor)
                if edge and not edge.is_blocked:
                    # Escolher métrica baseada no critério de otimização
                    if optimization_criterion == "time":
                        edge_cost = edge.time_minutes
                    else:  # distance (default)
                        edge_cost = edge.distance_km
                    
                    # g: custo do caminho até aqui (soma direta dos custos das arestas)
                    edge_distance = edge.distance_km  # Sempre usar distância para autonomia
                    new_g = g_current + edge_cost
                    new_distance = distance + edge_distance
                    
                    # h: heurística até o destino (euclidiana como estimativa mínima)
                    neighbor_node = graph.get_node_by_id(neighbor)
                    if neighbor_node:
                        h = TaxiHeuristics.euclidean_distance_heuristic(
                            (neighbor_node.position.longitude, neighbor_node.position.latitude),
                            (end_node.position.longitude, end_node.position.latitude)
                        )
                        
                        # Ajustar escala da heurística baseado no critério
                        # Para tempo: heurística em graus precisa ser convertida aproximadamente para minutos
                        # Para distância: heurística em graus já está em escala similar
                        if optimization_criterion == "time":
                            # Aproximação: 1 grau ≈ 111 km, velocidade média ≈ 50 km/h
                            # Então 1 grau ≈ 111/50 * 60 ≈ 133 minutos
                            h = h * 133.0
                    else:
                        h = 0
                    
                    # Adicionar penalidade ambiental se houver request (pequena influência)
                    if request:
                        env_penalty = TaxiHeuristics.environmental_heuristic(vehicle, request)
                        if env_penalty == float('inf'):
                            continue
                        h += abs(env_penalty) * 0.01
                    
                    f = new_g + h
                    
                    # Se não visitamos ou encontramos melhor caminho
                    if neighbor not in g_score or new_g < g_score[neighbor]:
                        if new_distance <= vehicle.max_autonomy:
                            g_score[neighbor] = new_g
                            heapq.heappush(
                                heap,
                                (f, new_g, neighbor, path + [neighbor], new_distance)
                            )
    
    return None, float('inf')


def a_star_find_station(graph, start_node_id: int, station_type: str,
                       vehicle: Vehicle) -> Tuple[Optional[List[int]], float]:
    """
    A* para encontrar estação de recarga/abastecimento
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param station_type: Tipo de estação
    :param vehicle: Veículo
    :return: (path, cost)
    """
    
    heap = [(0.0, 0.0, start_node_id, [start_node_id], 0.0)]
    visited = set()
    
    while heap:
        f_score, g_current, current_node, path, distance = heapq.heappop(heap)
        
        if current_node in visited:
            continue
        visited.add(current_node)
        
        # Verificar se é estação
        node = graph.get_node_by_id(current_node)
        if node and node.type == station_type:
            return path, g_current
        
        # Expandir vizinhos
        for neighbor in graph.get_neighbors(current_node):
            if neighbor not in visited:
                edge = graph.get_edge(current_node, neighbor)
                if edge and not edge.is_blocked:
                    edge_distance = edge.distance_km
                    new_g = g_current + edge_distance
                    new_distance = distance + edge_distance
                    
                    # Heurística: distância até o nó mais próximo
                    h = 0
                    for candidate_node_id, candidate_node in graph.nodes.items():
                        if candidate_node.type == station_type:
                            h = graph.get_distance_between_nodes(neighbor, candidate_node_id)
                            break
                    
                    f = new_g + h
                    
                    if new_distance <= vehicle.max_autonomy + 5:  # Margem de segurança
                        heapq.heappush(
                            heap,
                            (f, new_g, neighbor, path + [neighbor], new_distance)
                        )
    
    return None, float('inf')


def a_star_optimize_route(graph, waypoints: List[int],
                         vehicle: Vehicle) -> Tuple[Optional[List[int]], float]:
    """
    A* para otimizar rota visitando múltiplos waypoints
    """
    
    if not waypoints or len(waypoints) < 2:
        return waypoints, 0.0
    
    total_path = [waypoints[0]]
    total_cost = 0.0
    
    for i in range(len(waypoints) - 1):
        path, cost = a_star_find_path(graph, waypoints[i], waypoints[i+1], vehicle)
        if path:
            total_path.extend(path[1:])  # Evitar duplicação de nós
            total_cost += cost
        else:
            return None, float('inf')
    
    return total_path, total_cost
