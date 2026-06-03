"""
Greedy Search - Procura informada gulosa
"""

from typing import List, Tuple, Optional
from models import Vehicle, TransportRequest
from algorithms.informed.heuristics import TaxiHeuristics


def greedy_allocate_request(fleet: List[Vehicle], request: TransportRequest,
                           graph, start_node_id: int, end_node_id: int) -> Tuple[Optional[int], Optional[List[int]], float]:
    """
    Procura Greedy para alocar pedido ao melhor veículo imediato
    Seleciona o veículo com melhor score heurístico instantâneo
    
    :param fleet: Lista de veículos
    :param request: Pedido de transporte
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :return: (vehicle_id, path, score)
    """
    
    best_vehicle = None
    best_score = float('inf')
    best_path = None
    
    for vehicle in fleet:
        if not vehicle.is_available():
            continue
        
        # Calcular distância ao destino
        distance = graph.get_distance_between_nodes(start_node_id, end_node_id)
        
        # Calcular score heurístico combinado
        score = TaxiHeuristics.combined_allocation_heuristic(
            vehicle, request, distance
        )
        
        if score < best_score:
            best_vehicle = vehicle.id
            best_score = score
            # BFS simples para encontrar um caminho
            best_path = graph.get_shortest_path_bfs(start_node_id, end_node_id)
    
    return best_vehicle, best_path, best_score


def greedy_find_cheapest_vehicle(fleet: List[Vehicle], 
                                 distance_km: float) -> Tuple[Optional[int], float]:
    """
    Greedy para encontrar veículo mais barato
    """
    
    best_vehicle = None
    best_cost = float('inf')
    
    for vehicle in fleet:
        if not vehicle.is_available():
            continue
        
        cost = distance_km * vehicle.operational_cost_per_km
        if cost < best_cost:
            best_vehicle = vehicle.id
            best_cost = cost
    
    return best_vehicle, best_cost


def greedy_find_available_vehicle(fleet: List[Vehicle],
                                  request: TransportRequest) -> Tuple[Optional[int], float]:
    """
    Greedy para encontrar primeiro veículo disponível
    """
    
    for vehicle in fleet:
        if (vehicle.is_available() and 
            vehicle.passenger_capacity >= request.num_passengers):
            return vehicle.id, 0.0
    
    return None, float('inf')


def greedy_nearest_vehicle(fleet: List[Vehicle], pickup_location,
                           graph, start_node_id: int) -> Tuple[Optional[int], float]:
    """
    Greedy para encontrar veículo mais próximo
    """
    
    best_vehicle = None
    best_distance = float('inf')
    
    for vehicle in fleet:
        if not vehicle.is_available():
            continue
        
        # Encontrar nó mais próximo da localização do veículo
        distance = graph.get_distance_between_nodes(
            start_node_id,
            start_node_id  # Simplificado
        )
        
        if distance < best_distance:
            best_vehicle = vehicle.id
            best_distance = distance
    
    return best_vehicle, best_distance


def greedy_path_search(graph, start_node_id: int, end_node_id: int,
                      max_steps: int = 20) -> Optional[List[int]]:
    """
    Greedy para encontrar caminho sempre escolhendo vizinho mais próximo
    """
    
    path = [start_node_id]
    current = start_node_id
    visited = {start_node_id}
    steps = 0
    
    while current != end_node_id and steps < max_steps:
        neighbors = graph.get_neighbors(current)
        best_neighbor = None
        best_distance = float('inf')
        
        for neighbor in neighbors:
            if neighbor not in visited:
                distance = graph.get_distance_between_nodes(neighbor, end_node_id)
                if distance < best_distance:
                    best_neighbor = neighbor
                    best_distance = distance
        
        if best_neighbor is None:
            break
        
        path.append(best_neighbor)
        visited.add(best_neighbor)
        current = best_neighbor
        steps += 1
    
    if current == end_node_id:
        return path
    return None


def greedy_find_path(graph, start_node_id: int, end_node_id: int,
                    vehicle: Vehicle, request: TransportRequest = None, 
                    optimization_criterion: str = "distance") -> Tuple[Optional[List[int]], float]:
    """
    Greedy Best-First Search para encontrar caminho
    Escolhe sempre o vizinho com menor heurística até o destino,
    considerando o custo da aresta (distância ou tempo)
    
    IMPORTANTE: Usa heap para explorar melhor o espaço de busca e evitar ficar preso
    
    :param graph: Grafo da cidade
    :param start_node_id: ID do nó de partida
    :param end_node_id: ID do nó de destino
    :param vehicle: Veículo que irá fazer a rota
    :param request: Pedido associado (opcional)
    :param optimization_criterion: "distance" para otimizar distância, "time" para otimizar tempo
    :return: (path, cost)
    """
    import heapq
    
    start_node = graph.get_node_by_id(start_node_id)
    end_node = graph.get_node_by_id(end_node_id)
    
    if not start_node or not end_node:
        return None, float('inf')
    
    if start_node_id == end_node_id:
        return [start_node_id], 0.0
    
    # Heap: (heuristic, total_cost, node_id, path, distance_traveled)
    # Greedy: ordena apenas por heurística (não considera custo acumulado)
    h_start = TaxiHeuristics.euclidean_distance_heuristic(
        (start_node.position.longitude, start_node.position.latitude),
        (end_node.position.longitude, end_node.position.latitude)
    )
    
    heap = [(h_start, 0.0, start_node_id, [start_node_id], 0.0)]
    visited = set()
    
    while heap:
        h_current, total_cost, current, path, distance_traveled = heapq.heappop(heap)
        
        # Se já visitamos este nó, pular
        if current in visited:
            continue
        
        visited.add(current)
        
        # Se chegamos ao destino
        if current == end_node_id:
            return path, total_cost
        
        # Verificar autonomia
        if distance_traveled > vehicle.max_autonomy:
            continue
        
        # Expandir vizinhos
        neighbors = graph.get_neighbors(current)
        for neighbor in neighbors:
            if neighbor not in visited:
                edge = graph.get_edge(current, neighbor)
                
                if edge and not edge.is_blocked:
                    # Calcular heurística até o destino
                    neighbor_node = graph.get_node_by_id(neighbor)
                    if neighbor_node:
                        # Calcular heurística euclidiana (distância em linha reta)
                        h_euclidean = TaxiHeuristics.euclidean_distance_heuristic(
                            (neighbor_node.position.longitude, neighbor_node.position.latitude),
                            (end_node.position.longitude, end_node.position.latitude)
                        )
                        
                        # Escolher métrica baseada no critério
                        if optimization_criterion == "time":
                            edge_cost = edge.time_minutes
                            # Para otimização de tempo, converter heurística euclidiana para tempo estimado
                            # Assumir velocidade média de 50 km/h → 1 grau ≈ 111 km ≈ 2.22 horas ≈ 133 minutos
                            h = h_euclidean * 133.0
                        else:
                            edge_cost = edge.distance_km
                            # Para otimização de distância, heurística já está em escala correta
                            # Converter para km: 1 grau ≈ 111 km
                            h = h_euclidean * 111.0
                        
                        new_cost = total_cost + edge_cost
                        new_distance = distance_traveled + edge.distance_km
                        
                        # GREEDY MELHORADO: h + peso significativo do custo da aresta
                        # Peso de 0.5 (50%) equilibra heurística com custo real
                        # Isso torna o Greedy mais consistente e próximo ao A* em qualidade
                        # mas mantém a característica de priorizar proximidade ao destino
                        h_adjusted = h + (edge_cost * 0.5)  # 50% de peso no custo da aresta
                        
                        # Verificar autonomia
                        if new_distance <= vehicle.max_autonomy:
                            heapq.heappush(
                                heap,
                                (h_adjusted, new_cost, neighbor, path + [neighbor], new_distance)
                            )
    
    return None, float('inf')
