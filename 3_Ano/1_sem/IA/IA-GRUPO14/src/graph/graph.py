"""
Classe principal CityGraph para gerenciar a estrutura do grafo da cidade
"""

from typing import Dict, List, Tuple, Optional
from collections import deque

try:
    from .models import Node, Edge
except ImportError:
    from graph.models import Node, Edge


class CityGraph:
    """Grafo representando a cidade"""
    
    def __init__(self):
        self.nodes: Dict[int, Node] = {}
        self.edges: Dict[Tuple[int, int], Edge] = {}
        self.adjacency_list: Dict[int, List[int]] = {}
    
    def add_node(self, node_id: int, name: str, position, 
                 node_type: str = "location", demand_level: str = "medium") -> Node:
        """Adiciona um nó ao grafo"""
        node = Node(node_id, name, position, node_type, demand_level)
        self.nodes[node_id] = node
        if node_id not in self.adjacency_list:
            self.adjacency_list[node_id] = []
        return node
    
    def add_edge(self, from_id: int, to_id: int, distance_km: float, 
                 time_minutes: float, bidirectional: bool = True):
        """Adiciona uma aresta ao grafo"""
        edge = Edge(from_id, to_id, distance_km, time_minutes)
        self.edges[(from_id, to_id)] = edge
        
        if from_id not in self.adjacency_list:
            self.adjacency_list[from_id] = []
        self.adjacency_list[from_id].append(to_id)
        
        if bidirectional:
            reverse_edge = Edge(to_id, from_id, distance_km, time_minutes)
            self.edges[(to_id, from_id)] = reverse_edge
            if to_id not in self.adjacency_list:
                self.adjacency_list[to_id] = []
            self.adjacency_list[to_id].append(from_id)
    
    def get_neighbors(self, node_id: int) -> List[int]:
        """Retorna vizinhos de um nó"""
        return self.adjacency_list.get(node_id, [])
    
    def get_edge(self, from_id: int, to_id: int) -> Optional[Edge]:
        """Obtém uma aresta específica"""
        return self.edges.get((from_id, to_id))
    
    def get_edge_distance(self, from_id: int, to_id: int) -> float:
        """Obtém distância de uma aresta"""
        edge = self.get_edge(from_id, to_id)
        if edge:
            return edge.distance_km
        return float('inf')
    
    def get_edge_time(self, from_id: int, to_id: int) -> float:
        """Obtém tempo de percurso de uma aresta"""
        edge = self.get_edge(from_id, to_id)
        if edge:
            return edge.get_effective_time()
        return float('inf')
    
    def set_traffic_condition(self, from_id: int, to_id: int, multiplier: float):
        """Define condição de trânsito para uma aresta"""
        edge = self.get_edge(from_id, to_id)
        if edge:
            edge.traffic_multiplier = multiplier
    
    def block_edge(self, from_id: int, to_id: int):
        """Bloqueia uma aresta"""
        edge = self.get_edge(from_id, to_id)
        if edge:
            edge.is_blocked = True
    
    def unblock_edge(self, from_id: int, to_id: int):
        """Desbloqueia uma aresta"""
        edge = self.get_edge(from_id, to_id)
        if edge:
            edge.is_blocked = False
    
    def get_all_nodes(self) -> List[Node]:
        """Retorna todos os nós"""
        return list(self.nodes.values())
    
    def get_node_by_id(self, node_id: int) -> Optional[Node]:
        """Obtém um nó pelo ID"""
        return self.nodes.get(node_id)
    
    def get_shortest_path_bfs(self, start_id: int, end_id: int) -> List[int]:
        """BFS para encontrar caminho mais curto (em número de nós)"""
        visited = set()
        queue = deque([(start_id, [start_id])])
        
        while queue:
            current, path = queue.popleft()
            if current == end_id:
                return path
            
            if current not in visited:
                visited.add(current)
                for neighbor in self.get_neighbors(current):
                    edge = self.get_edge(current, neighbor)
                    if not edge or not edge.is_blocked:
                        if neighbor not in visited:
                            queue.append((neighbor, path + [neighbor]))
        
        return []
    
    def get_distance_between_nodes(self, from_id: int, to_id: int) -> float:
        """Calcula distância entre dois nós (em linha reta)"""
        from_node = self.get_node_by_id(from_id)
        to_node = self.get_node_by_id(to_id)
        if from_node and to_node:
            return from_node.distance_to(to_node)
        return float('inf')
    
    def __repr__(self):
        return f"CityGraph({len(self.nodes)} nodes, {len(self.edges)} edges)"
