#!/usr/bin/env python3
"""
Este ficheiro mostra como integrar o sistema de animações
com um motor de simulação e algoritmos de procura.

"""

from typing import List, Dict, Set, Tuple
import time


class AnimatedSimulationEngine:
    """
    Motor de simulação com suporte a animações de procura nó-a-nó
    """

    def __init__(self, graph, viewer=None):
        self.graph = graph
        self.viewer = viewer  # Referência ao TaxiGreenViewer

        # Rastreamento para animações
        self.search_stats = {
            "current_node": None,
            "visited_nodes": [],
            "frontier_nodes": [],
            "explored_edges": [],
            "final_path": None,
            "search_active": False,
            "node_visit_times": {},  # Para visualização de timing
        }

    def animate_search_bfs(self, start_node, goal_node):
        """
        Busca BFS com animação nó-a-nó

        Mostra visualmente:
        • Nós sendo explorados (AMARELO)
        • Nós visitados (VERMELHO)
        • Nós na fronteira (OURO)
        • Caminho final (VERDE)
        """
        from collections import deque

        print(f"\n Iniciando BFS animado de {start_node} a {goal_node}")
        self.search_stats["search_active"] = True

        queue = deque([start_node])
        visited = set()
        parent = {start_node: None}

        # Animar nó inicial
        if self.viewer:
            self.viewer.animate_node_visit(start_node, "start")

        iteration = 0
        while queue:
            iteration += 1
            node = queue.popleft()

            if node in visited:
                continue

            # Atualizar stats
            self.search_stats["current_node"] = node
            self.search_stats["node_visit_times"][node] = time.time()

            # ANIMAR: Nó sendo explorado
            if self.viewer:
                self.viewer.animate_node_visit(node, "exploring")
                print(f"  [{iteration}] Explorando: {node}")

            visited.add(node)
            self.search_stats["visited_nodes"].append(node)

            # Verificar se é o objetivo
            if node == goal_node:
                print(f"  ✓ Objetivo encontrado: {goal_node}")

                # ANIMAR: Objetivo
                if self.viewer:
                    self.viewer.animate_node_visit(node, "goal")

                # Reconstruir caminho
                path = []
                curr = goal_node
                while curr:
                    path.append(curr)
                    curr = parent.get(curr)
                path.reverse()

                self.search_stats["final_path"] = path

                # ANIMAR: Caminho final
                if self.viewer:
                    print(f" Caminho: {' → '.join(path)}")
                    self.viewer.draw_visited_path(path, "lime", 4)

                self.search_stats["search_active"] = False
                return path

            # ANIMAR: Nó como visitado
            if self.viewer:
                self.viewer.animate_node_visit(node, "visited")

            # Explorar vizinhos
            neighbors = (
                self.graph.get_neighbors(node)
                if hasattr(self.graph, "get_neighbors")
                else []
            )

            for neighbor in neighbors:
                if neighbor not in visited:
                    # ANIMAR: Nó na fronteira
                    if self.viewer:
                        self.viewer.animate_node_visit(neighbor, "exploring")

                    parent[neighbor] = node
                    queue.append(neighbor)
                    self.search_stats["frontier_nodes"].append(neighbor)

            # Atualizar visualização
            if self.viewer:
                self.viewer.draw_graph_on_canvas()
                # Pequeno delay para ver a animação
                self.viewer.root.after(200)  # Esperar 200ms

        print(" Nenhum caminho encontrado")
        self.search_stats["search_active"] = False
        return None

    def animate_search_a_star(self, start_node, goal_node, heuristic_fn):
        """
        Busca A* com animação nó-a-nó

        Mostra:
        • Nós da fronteira em ordem de f(n) = g(n) + h(n)
        • Nós visitados em vermelho
        • Progresso da procura em tempo real
        """
        import heapq

        print(f"\n Iniciando A* animado de {start_node} a {goal_node}")
        self.search_stats["search_active"] = True

        # Conjuntos de controle
        open_set = [(0, start_node)]
        came_from = {}
        g_score = {start_node: 0}
        f_score = {start_node: heuristic_fn(start_node, goal_node)}

        # Animar início
        if self.viewer:
            self.viewer.animate_node_visit(start_node, "start")

        iteration = 0
        while open_set:
            iteration += 1
            current_f, current = heapq.heappop(open_set)

            # Atualizar stats
            self.search_stats["current_node"] = current
            self.search_stats["node_visit_times"][current] = time.time()

            # ANIMAR: Nó sendo explorado
            if self.viewer:
                self.viewer.animate_node_visit(current, "exploring")
                print(f"  [{iteration}] Explorando: {current} (f={current_f:.1f})")

            if current == goal_node:
                print(f" Objetivo encontrado: {goal_node}")

                # ANIMAR: Objetivo
                if self.viewer:
                    self.viewer.animate_node_visit(current, "goal")

                # Reconstruir caminho
                path = [current]
                while current in came_from:
                    current = came_from[current]
                    path.append(current)
                path.reverse()

                self.search_stats["final_path"] = path

                # ANIMAR: Caminho final
                if self.viewer:
                    print(f" Caminho: {' → '.join(path)}")
                    print(f" Custo total: {g_score[goal_node]:.1f}")
                    self.viewer.draw_visited_path(path, "#00ff88", 5)

                self.search_stats["search_active"] = False
                return path

            # ANIMAR: Nó como visitado
            if self.viewer:
                self.viewer.animate_node_visit(current, "visited")

            self.search_stats["visited_nodes"].append(current)

            # Explorar vizinhos
            neighbors = (
                self.graph.get_neighbors(current)
                if hasattr(self.graph, "get_neighbors")
                else []
            )

            for neighbor in neighbors:
                # Custo: distância para vizinho
                tentative_g = g_score[current] + self.graph.distance(current, neighbor)

                if neighbor not in g_score or tentative_g < g_score[neighbor]:
                    came_from[neighbor] = current
                    g_score[neighbor] = tentative_g
                    f_score[neighbor] = tentative_g + heuristic_fn(neighbor, goal_node)
                    heapq.heappush(open_set, (f_score[neighbor], neighbor))

                    # ANIMAR: Nó na fronteira
                    if self.viewer:
                        self.viewer.animate_node_visit(neighbor, "exploring")

                    self.search_stats["frontier_nodes"].append(neighbor)

            # Atualizar visualização
            if self.viewer:
                self.viewer.draw_graph_on_canvas()
                self.viewer.root.after(200)

        print("  ✗ Nenhum caminho encontrado")
        self.search_stats["search_active"] = False
        return None

    def print_search_statistics(self):
        """Imprime estatísticas da procura"""
        stats = self.search_stats
        print("\n Estatísticas da Procura:")
        print(f"  • Nós visitados: {len(stats['visited_nodes'])}")
        print(f"  • Nós na fronteira: {len(stats['frontier_nodes'])}")
        print(f"  • Caminho encontrado: {stats['final_path']}")
        if stats["final_path"]:
            print(f"  • Comprimento do caminho: {len(stats['final_path'])} nós")

        # Tempo por nó
        if stats["node_visit_times"]:
            times = list(stats["node_visit_times"].values())
            if len(times) > 1:
                avg_time = sum(t - times[0] for t in times[1:]) / (len(times) - 1)
                print(f"  • Tempo médio por nó: {avg_time * 1000:.1f}ms")
