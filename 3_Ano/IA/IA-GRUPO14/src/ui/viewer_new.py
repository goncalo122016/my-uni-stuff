"""
TaxiGreen - Animador de Carros Nó-a-Nó com Análise de Algoritmos
Seleção de algoritmo, velocidade, lista de carros e relatório detalhado
"""

import tkinter as tk
from tkinter import ttk, messagebox, scrolledtext
import time
from math import sin, cos, sqrt
from collections import deque
from os import path
from datetime import datetime

try:
    from PIL import Image, ImageTk
except ImportError:
    Image = None
    ImageTk = None

# Importar algoritmos
try:
    from algorithms.informed.a_star import a_star_find_path
    from algorithms.informed.greedy import greedy_find_path

    ALGORITHMS_AVAILABLE = True
except ImportError as e:
    print(f"Erro ao importar algoritmos: {e}")
    a_star_find_path = None
    greedy_find_path = None
    ALGORITHMS_AVAILABLE = False

# Importar gerenciador de trânsito
try:
    from ui.traffic_manager import TrafficConditionManager

    TRAFFIC_AVAILABLE = True
except ImportError as e:
    print(f"Aviso: Gerenciador de trânsito não disponível: {e}")
    TrafficConditionManager = None
    TRAFFIC_AVAILABLE = False

# Importar simulação automática
try:
    from simulation.auto_simulation import AutoSimulationEngine

    AUTO_SIMULATION_AVAILABLE = True
except ImportError as e:
    print(f"Aviso: Simulação automática não disponível: {e}")
    AutoSimulationEngine = None
    AUTO_SIMULATION_AVAILABLE = False


class ReportGenerator:
    """Gera relatórios simples de simulação"""

    def __init__(
        self,
        simulation_engine,
        algorithm_name,
        speed,
        vehicle=None,
        animation_path=None,
    ):
        self.engine = simulation_engine
        self.algorithm = algorithm_name
        self.speed = speed
        self.state = simulation_engine.state
        self.vehicle = vehicle  # Veículo específico (ou None para toda a frota)
        self.animation_path = animation_path or []  # Caminho percorrido

    def _calculate_path_distance(self):
        """Calcula distância real do caminho percorrido pelos nós
        Usa as distâncias do grafo entre nós adjacentes"""
        # Se não há movimento real (menos de 2 nós), retorna 0
        if not self.animation_path or len(self.animation_path) < 2:
            return 0.0

        total_distance = 0.0
        graph = self.state.graph

        # Iterar por cada par de nós consecutivos no caminho
        for i in range(len(self.animation_path) - 1):
            node_id1 = self.animation_path[i]
            node_id2 = self.animation_path[i + 1]

            # Procurar a edge entre estes dois nós no grafo
            # graph.edges é um dicionário com chave (from_node, to_node)
            edge = graph.get_edge(node_id1, node_id2)

            if edge:
                # Edge encontrada, usar sua distância
                total_distance += edge.distance_km
            else:
                # Se não encontrar edge, usar Euclidiana como fallback
                if node_id1 in graph.nodes and node_id2 in graph.nodes:
                    node1 = graph.nodes[node_id1]
                    node2 = graph.nodes[node_id2]
                    dx = node2.position.longitude - node1.position.longitude
                    dy = node2.position.latitude - node1.position.latitude
                    total_distance += sqrt(dx * dx + dy * dy)

        return total_distance

    def generate_report(self):
        """Gera relatório formatado - do veículo ou de toda a frota"""

        if self.vehicle:
            # Relatório individual do veículo
            return self._generate_vehicle_report()
        else:
            # Relatório da frota completa
            return self._generate_fleet_report()

    def _generate_vehicle_report(self):
        """Relatório de um veículo específico"""
        v = self.vehicle

        # Calcular distância da reprodução (do caminho real com base nos nós)
        distance = self._calculate_path_distance()

        # Se distância for 0, é porque não há movimento real (carro já está no destino)
        # Não usar fallback - mostrar 0 km com análise apropriada

        empty_distance = distance * 0.15  # 15% sem passageiros
        passengers_distance = distance - empty_distance

        # Calcular CO2
        if v.fuel_type.value == "electric":
            co2_emissions = distance * 50 / 1000  # 50g/km
            fuel_type_display = "Elétrico"
        else:
            co2_emissions = distance * 120 / 1000  # 120g/km
            fuel_type_display = "Combustão"

        # Financeira
        cost_per_km = v.operational_cost_per_km
        total_cost = distance * cost_per_km

        # Timestamp dinâmico
        from datetime import datetime

        timestamp = datetime.now().strftime("%d/%m/%Y %H:%M:%S")

        report = f"""
╔════════════════════════════════════════════════════════════════════════════════╗
║                RELATÓRIO DE SIMULAÇÃO - TaxiGreen (VEÍCULO)                    ║
╚════════════════════════════════════════════════════════════════════════════════╝

CONFIGURAÇÃO
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Algoritmo      : {self.algorithm}
  Velocidade     : {self.speed:.1f}x
  Data/Hora      : {timestamp}

VEÍCULO
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ID             : {v.id}
  Tipo           : {v.type.value.upper()}
  Combustível    : {fuel_type_display}
  Passageiros    : {v.current_passengers}/{v.passenger_capacity}
  Autonomia      : {v.current_autonomy:.1f}/{v.max_autonomy:.1f} km
  Status         : {v.status.value.upper()}

DISTÂNCIAS (Esta Reprodução)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Distância Total         : {distance:.2f} km
    • Com Passageiros     : {passengers_distance:.2f} km ({100 - (empty_distance / distance * 100 if distance > 0 else 0):.1f}%)
    • Sem Passageiros     : {empty_distance:.2f} km ({empty_distance / distance * 100 if distance > 0 else 0:.1f}%)

ANÁLISE FINANCEIRA (Esta Reprodução)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Custo por km            : €{cost_per_km:.4f}
  Custo Total (reprodução): €{total_cost:.2f}
  Lucro Estimado (20% margem) : €{total_cost * 0.20:.2f}

ANÁLISE AMBIENTAL & CO₂ (Esta Reprodução)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Emissões de CO₂         : {co2_emissions:.3f} kg
  Taxa de Emissão        : {50 if v.fuel_type.value == "electric" else 120} g/km
  Impacto Ambiental      : {"Baixo " if v.fuel_type.value == "electric" else "Moderado"}
  Equivalente Árvores     : {co2_emissions / 0.021:.1f} árvores/ano necessárias

╔════════════════════════════════════════════════════════════════════════════════╗
║                          FIM DO RELATÓRIO                                      ║
╚════════════════════════════════════════════════════════════════════════════════╝
"""
        return report

    def _generate_fleet_report(self):
        """Relatório completo da frota"""
        vehicles = self.state.vehicles

        total_distance = sum(v.total_distance_km for v in vehicles)
        empty_distance = sum(v.empty_distance_km for v in vehicles)
        total_cost = sum(v.total_cost for v in vehicles)

        # CO2
        electric_vehicles = [v for v in vehicles if v.fuel_type.value == "electric"]
        combustion_vehicles = [v for v in vehicles if v.fuel_type.value == "combustion"]

        electric_distance = sum(v.total_distance_km for v in electric_vehicles)
        combustion_distance = sum(v.total_distance_km for v in combustion_vehicles)

        co2_combustion = combustion_distance * 120 / 1000
        co2_electric = electric_distance * 50 / 1000
        total_co2 = co2_combustion + co2_electric

        report = f"""
╔════════════════════════════════════════════════════════════════════════════════╗
║                RELATÓRIO DE SIMULAÇÃO - TaxiGreen (FROTA)                      ║
╚════════════════════════════════════════════════════════════════════════════════╝

CONFIGURAÇÃO
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Algoritmo      : {self.algorithm}
  Velocidade     : {self.speed:.1f}x
  Data/Hora      : {datetime.now().strftime("%d/%m/%Y %H:%M:%S")}

ANÁLISE DA FROTA
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Total de Veículos       : {len(vehicles)}
    • Elétricos           : {len(electric_vehicles)}
    • Combustão           : {len(combustion_vehicles)}

  Distância Total         : {total_distance:.2f} km
    • Com Passageiros     : {total_distance - empty_distance:.2f} km ({100 - (empty_distance / total_distance * 100 if total_distance > 0 else 0):.1f}%)
    • Sem Passageiros     : {empty_distance:.2f} km ({empty_distance / total_distance * 100 if total_distance > 0 else 0:.1f}%)

ANÁLISE FINANCEIRA
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Custo Operacional Total : €{total_cost:.2f}
  Custo Médio por km      : €{total_cost / total_distance if total_distance > 0 else 0:.4f}

ANÁLISE AMBIENTAL & CO₂
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  Emissões de CO₂ Total   : {total_co2:.3f} kg
    • De Combustão        : {co2_combustion:.3f} kg
    • De Eletricidade     : {co2_electric:.3f} kg

  Distância Elétrica      : {electric_distance:.2f} km ({electric_distance / total_distance * 100 if total_distance > 0 else 0:.1f}%)
  Distância Combustão     : {combustion_distance:.2f} km ({combustion_distance / total_distance * 100 if total_distance > 0 else 0:.1f}%)

╔════════════════════════════════════════════════════════════════════════════════╗
║                          FIM DO RELATÓRIO                                      ║
╚════════════════════════════════════════════════════════════════════════════════╝
"""
        return report


class VehicleListWindow:
    """Janela com listagem detalhada de veículos"""

    def __init__(self, parent, vehicles):
        self.window = tk.Toplevel(parent)
        self.window.title("Lista de Veículos")
        self.window.geometry("1000x500")

        tree_frame = ttk.Frame(self.window)
        tree_frame.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)

        scrollbar = ttk.Scrollbar(tree_frame)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)

        self.tree = ttk.Treeview(
            tree_frame,
            columns=(
                "id",
                "name",
                "type",
                "fuel",
                "status",
                "autonomy",
                "passengers",
                "cost_km",
                "total_distance",
                "empty_distance",
                "total_cost",
            ),
            height=20,
            yscrollcommand=scrollbar.set,
        )
        scrollbar.config(command=self.tree.yview)

        self.tree.column("#0", width=5)
        self.tree.column("id", width=40, anchor=tk.CENTER)
        self.tree.column("name", width=120)
        self.tree.column("type", width=80)
        self.tree.column("fuel", width=80)
        self.tree.column("status", width=80)
        self.tree.column("autonomy", width=100, anchor=tk.CENTER)
        self.tree.column("passengers", width=80, anchor=tk.CENTER)
        self.tree.column("cost_km", width=80, anchor=tk.CENTER)
        self.tree.column("total_distance", width=100, anchor=tk.CENTER)
        self.tree.column("empty_distance", width=100, anchor=tk.CENTER)
        self.tree.column("total_cost", width=80, anchor=tk.CENTER)

        self.tree.heading("#0", text="")
        self.tree.heading("id", text="ID")
        self.tree.heading("name", text="Nome")
        self.tree.heading("type", text="Tipo")
        self.tree.heading("fuel", text="Combustível")
        self.tree.heading("status", text="Status")
        self.tree.heading("autonomy", text="Autonomia")
        self.tree.heading("passengers", text="Passageiros")
        self.tree.heading("cost_km", text="€/km")
        self.tree.heading("total_distance", text="Dist. Total")
        self.tree.heading("empty_distance", text="Dist. Vazia")
        self.tree.heading("total_cost", text="Custo Total")

        for vehicle in vehicles:
            values = (
                vehicle.id,
                vehicle.name,
                vehicle.type.value.upper(),
                vehicle.fuel_type.value.upper(),
                vehicle.status.value.upper(),
                f"{vehicle.current_autonomy:.1f}/{vehicle.max_autonomy:.1f}",
                f"{vehicle.current_passengers}/{vehicle.passenger_capacity}",
                f"€{vehicle.operational_cost_per_km:.3f}",
                f"{vehicle.total_distance_km:.2f}",
                f"{vehicle.empty_distance_km:.2f}",
                f"€{vehicle.total_cost:.2f}",
            )
            self.tree.insert("", tk.END, values=values)

        self.tree.pack(fill=tk.BOTH, expand=True)


class ConfigurationWindow:
    """Janela de configuração de algoritmo e velocidade"""

    def __init__(self, parent, callback):
        self.window = tk.Toplevel(parent)
        self.window.title("Configuração")
        self.window.geometry("400x250")
        self.window.resizable(False, False)
        self.callback = callback

        algo_frame = ttk.LabelFrame(self.window, text="Algoritmo", padding=15)
        algo_frame.pack(fill=tk.X, padx=20, pady=10)

        self.algorithm_var = tk.StringVar(value="BFS")

        algorithms = [
            ("BFS - Busca em Largura", "BFS"),
            ("DFS - Busca em Profundidade", "DFS"),
            ("UCS - Busca Uniforme", "UCS"),
            ("IDS - Busca Iterativa", "IDS"),
            ("Greedy - Guloso", "GREEDY"),
            ("A* - A-Star", "A_STAR"),
        ]

        for label, value in algorithms:
            ttk.Radiobutton(
                algo_frame,
                text=label,
                variable=self.algorithm_var,
                value=value,
                command=self._auto_apply,
            ).pack(anchor=tk.W, pady=2)

        # Critério de otimização (para A* e Greedy)
        criterion_frame = ttk.LabelFrame(
            self.window, text="Critério de Otimização", padding=15
        )
        criterion_frame.pack(fill=tk.X, padx=20, pady=10)

        self.criterion_var = tk.StringVar(value="distance")

        ttk.Label(
            criterion_frame,
            text="Para algoritmos informados (A*, Greedy):",
            font=("Arial", 9, "italic"),
        ).pack(anchor=tk.W, pady=2)
        ttk.Radiobutton(
            criterion_frame,
            text="Distância (km) - Caminho mais curto",
            variable=self.criterion_var,
            value="distance",
            command=self._auto_apply,
        ).pack(anchor=tk.W, pady=2)
        ttk.Radiobutton(
            criterion_frame,
            text="Tempo (min) - Caminho mais rápido",
            variable=self.criterion_var,
            value="time",
            command=self._auto_apply,
        ).pack(anchor=tk.W, pady=2)

        speed_frame = ttk.LabelFrame(self.window, text="Velocidade", padding=15)
        speed_frame.pack(fill=tk.X, padx=20, pady=10)

        self.speed_var = tk.DoubleVar(value=1.0)
        ttk.Scale(
            speed_frame,
            from_=0.5,
            to=5.0,
            orient=tk.HORIZONTAL,
            variable=self.speed_var,
            command=self._update_label,
        ).pack(fill=tk.X)

        self.speed_label = ttk.Label(speed_frame, text="1.0x")
        self.speed_label.pack()

        button_frame = ttk.Frame(self.window)
        button_frame.pack(fill=tk.X, padx=20, pady=20)

        ttk.Button(button_frame, text="Cancelar", command=self.window.destroy).pack(
            side=tk.LEFT, padx=5
        )

    def _update_label(self, value):
        self.speed_label.config(text=f"{float(value):.1f}x")

    def _auto_apply(self):
        """Aplica configuração e fecha automaticamente"""
        self.callback(
            algorithm=self.algorithm_var.get(),
            speed=self.speed_var.get(),
            criterion=self.criterion_var.get(),
        )
        self.window.destroy()


class TaxiGreenViewerAnimated:
    """
    Visualizador com Animação de Carros Nó-a-Nó
    - Mapa 100% da largura no topo
    - Painel de controles 100% da largura na base
    - Seleção de carro e destino
    - Animação suave entre nós
    """

    def __init__(self, root, simulation_engine):
        self.root = root
        self.engine = simulation_engine
        self.state = simulation_engine.state

        self.root.title("TaxiGreen - Animação de Carros Nó-a-Nó")
        self.root.geometry("1800x1000")
        self.root.minsize(1200, 800)

        # === GERENCIADOR DE TRÂNSITO ===
        self.traffic_manager = None
        self.show_traffic_conditions = tk.BooleanVar(
            value=True
        )  # ATIVADO POR PADRÃO PARA TESTE
        if TRAFFIC_AVAILABLE and TrafficConditionManager:
            self.traffic_manager = TrafficConditionManager(
                start_hour=6
            )  # Começa às 6 da manhã
            print("Traffic Manager inicializado com sucesso!")
        else:
            print("Traffic Manager NÃO disponível!")

        self.auto_simulation = None
        if AUTO_SIMULATION_AVAILABLE and AutoSimulationEngine:
            print(
                f"🔧 Inicializando AutoSimulation com {len(self.state.vehicles)} veículos",
                flush=True,
            )

            def algorithm_finder_wrapper(start, end):
                """Wrapper que usa veículo dummy para pathfinding"""
                # Usar primeiro veículo disponível ou criar dummy
                vehicle = self.state.vehicles[0] if self.state.vehicles else None
                return self.find_path_with_algorithm(start, end, vehicle)

            self.auto_simulation = AutoSimulationEngine(
                vehicles=self.state.vehicles,
                graph=self.state.graph,
                stations=self.state.stations,  # Passar estações
                algorithm_finder=algorithm_finder_wrapper,
            )
            print(" Simulação Automática disponível!", flush=True)
        else:
            print(" Simulação Automática NÃO disponível!", flush=True)

        # === CONFIGURAÇÕES ===
        self.algorithm = "BFS"
        self.animation_speed = 1.0
        self.optimization_criterion = "distance"  # "distance" ou "time"
        self.current_dataset = tk.StringVar()  # Dataset/grafo atual

        # === ESTADO DE ANIMAÇÃO ===
        self.animating = False
        self.animation_start_time = 0
        self.last_animation_time = 0  # Para rastrear tempo simulado
        self.auto_sim_last_time = 0  # Para rastrear tempo na simulação automática
        self.animation_pause_time = 0  # Tempo quando pausou para reabastecer
        self.animation_path = []  # Caminho [node_id1, node_id2, ...]
        self.animation_duration = 1.5  # segundos por nó
        self.vehicle_in_animation = None
        self.refueling_start_time = None  # Tempo de início do reabastecimento
        self.target_station = None  # Estação alvo quando indo reabastecer

        # === ESTADO DE TEMPO DA VIAGEM (baseado em grafo) ===
        self.journey_total_time_minutes = 0  # Tempo total da viagem em minutos
        self.journey_elapsed_time_minutes = 0  # Tempo decorrido na viagem em minutos
        self.journey_start_time = "06:00"  # Hora de início da viagem

        # === ESTADO DO VEÍCULO E DESTINO ===
        self.selected_vehicle = tk.StringVar()
        self.selected_destination = tk.StringVar()

        # === CACHE DE IMAGENS ===
        self.vehicle_images = {}
        self.load_vehicle_images()

        # === COORDENADAS ESCALADAS (cache) ===
        self.scaled_positions = {}
        self.canvas_width = 1600
        self.canvas_height = 800

        # === MENU BAR ===
        self._setup_menubar()

        self.setup_ui()

    def _setup_menubar(self):
        """Cria menu bar com opções"""
        menubar = tk.Menu(self.root)
        self.root.config(menu=menubar)

        # Menu Simulação
        sim_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Simulação", menu=sim_menu)
        sim_menu.add_command(label="Configurações...", command=self._open_config)
        sim_menu.add_separator()
        sim_menu.add_command(label="Sair", command=self.root.quit)

        # Menu Visualização
        view_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="Visualização", menu=view_menu)
        view_menu.add_command(
            label="Lista de Carros...", command=self._show_vehicle_list
        )

        # Adicionar toggle de trânsito se disponível
        if TRAFFIC_AVAILABLE and self.traffic_manager:
            view_menu.add_separator()
            view_menu.add_checkbutton(
                label="🚦 Condições de Trânsito",
                variable=self.show_traffic_conditions,
                command=self._toggle_traffic_conditions,
            )

        # Menu Relatórios
        rel_menu = tk.Menu(menubar, tearoff=0)
        menubar.add_cascade(label="📊 Relatórios", menu=rel_menu)
        rel_menu.add_command(label="Gerar Relatório...", command=self._show_report)

    def _open_config(self):
        """Abre janela de configuração"""
        ConfigurationWindow(self.root, self._apply_config)

    def _apply_config(self, algorithm, speed, criterion="distance"):
        """Aplica configurações"""
        self.algorithm = algorithm
        self.animation_speed = speed
        self.optimization_criterion = criterion
        self.animation_duration = 1.5 / speed

        criterion_text = "Distância" if criterion == "distance" else "Tempo"
        self.info_label.config(
            text=f"Configuração: {algorithm} a {speed:.1f}x | Otimização: {criterion_text}"
        )

    def _show_vehicle_list(self):
        """Mostra lista de carros"""
        VehicleListWindow(self.root, self.state.vehicles)

    def _toggle_traffic_conditions(self):
        """Ativa/desativa visualização de condições de trânsito"""
        print(f"Toggle trânsito: {self.show_traffic_conditions.get()}")
        print(f" Traffic manager existe: {self.traffic_manager is not None}")

        if self.show_traffic_conditions.get():
            self._initialize_traffic()
        else:
            self.info_label.config(text="Condições de trânsito desativadas")
        self.draw_graph()

    def _initialize_traffic(self):
        """Inicializa condições de trânsito"""
        if not self.traffic_manager:
            return

        all_edges = [
            (edge.from_node, edge.to_node) for edge in self.state.graph.edges.values()
        ]
        print(f" Total de arestas: {len(all_edges)}")

        # Forçar criação de condições em TODAS as arestas (para debug)
        conditions_created = 0
        for edge_tuple in all_edges:
            # Chamar get_traffic_multiplier para forçar criação
            multiplier = self.traffic_manager.get_traffic_multiplier(
                edge_tuple[0], edge_tuple[1]
            )
            if multiplier > 1.0:
                conditions_created += 1
                print(
                    f" Criada condição na aresta {edge_tuple}: multiplicador={multiplier:.2f}"
                )

        print(f"🚦 Condições criadas: {conditions_created}")

        # Mostrar estatísticas
        stats = self.traffic_manager.get_statistics()
        print(f"Estatísticas: {stats}")
        msg = f"Trânsito ativado: {stats['livre']}🟢 {stats['moderado']}🟡 {stats['pesado']}🟠 {stats['engarrafado']}🔴"
        self.info_label.config(text=msg)

    def _update_traffic_loop(self):
        """Loop de atualização de condições de trânsito"""
        if self.traffic_manager and self.show_traffic_conditions.get():
            # No novo sistema, tráfego é determinístico por hora
            # Não precisa atualizar aleatoriamente
            # O draw_graph() já mostra as condições corretas baseadas na hora

            # Apenas redesenhar para mostrar hora atualizada
            self.draw_graph()

        # Continuar loop a cada 30 segundos
        self.root.after(30000, self._update_traffic_loop)  # 30 segundos = 30000ms

    def _show_report(self):
        """Mostra relatório"""
        # Se há veículo em animação, mostrar relatório desse veículo
        # Senão, mostrar relatório da frota completa
        vehicle = self.vehicle_in_animation if self.vehicle_in_animation else None
        animation_path = (
            self.animation_path if self.animating or self.animation_path else []
        )

        report_gen = ReportGenerator(
            self.engine,
            self.algorithm,
            self.animation_speed,
            vehicle=vehicle,
            animation_path=animation_path,
        )
        report_text = report_gen.generate_report()

        report_window = tk.Toplevel(self.root)
        report_window.title(" Relatório de Simulação")
        report_window.geometry("900x600")

        text_widget = scrolledtext.ScrolledText(
            report_window, font=("Courier", 9), bg="#1e1e1e", fg="#00ff00", wrap=tk.WORD
        )
        text_widget.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        text_widget.insert(tk.END, report_text)
        text_widget.config(state=tk.DISABLED)

    def load_vehicle_images(self):
        """Carrega imagens de veículos dos assets"""
        if Image is None:
            print("PIL não disponível - usando formas geométricas")
            return

        assets_dir = path.join(path.dirname(__file__), "..", "assets", "images")

        # Mapeamento de tipo de veículo para ícone
        vehicle_types = {
            "truck": "truck.png",
            "van": "truck.png",  # usar mesmo ícone
            "electric": "vehicle_electric.png",
            "combustion": "vehicle_combustion.png",
        }

        for type_name, filename in vehicle_types.items():
            try:
                img_path = path.join(assets_dir, filename)
                img = Image.open(img_path)
                img = img.resize(
                    (24, 24),
                    Image.Resampling.LANCZOS
                    if hasattr(Image, "Resampling")
                    else Image.LANCZOS,
                )
                self.vehicle_images[type_name] = ImageTk.PhotoImage(img)
            except Exception as e:
                print(f"Não carregou {filename}: {e}")

    def toggle_auto_simulation(self):
        """Inicia ou para a simulação automática"""
        if not self.auto_simulation:
            messagebox.showwarning("Aviso", "Simulação automática não disponível!")
            return

        if self.auto_simulation.is_running:
            # Parar simulação
            self.auto_simulation.stop_simulation()
            self.auto_sim_button.config(text="Simulação Automática")
            self.info_label.config(text="Simulação automática parada")

            # Mostrar relatório final
            self.root.after(500, self._show_auto_simulation_report)
        else:
            # Iniciar simulação
            self.auto_simulation.start_simulation()
            self.auto_sim_button.config(text="Parar Simulação")
            self.info_label.config(
                text="Simulação automática iniciada! Gerando pedidos..."
            )

            # Iniciar loop de atualização
            self.root.after(1000, self._update_auto_simulation_loop)

    def reset_auto_simulation(self):
        """Reseta a simulação automática"""
        if not self.auto_simulation:
            messagebox.showwarning("Aviso", "Simulação automática não disponível!")
            return

        # Confirmar reset
        if messagebox.askyesno(
            "Confirmar Reset",
            "Deseja resetar a simulação automática?\n\nIsso irá:\n• Parar a simulação\n• Limpar todos os pedidos\n• Resetar veículos para 100% autonomia\n• Limpar todas as métricas",
        ):
            # Resetar simulação
            self.auto_simulation.reset_simulation()

            # Resetar relógio para 06:00
            if self.traffic_manager:
                self.traffic_manager.current_hour = 6
                self.traffic_manager.current_minute = 0

            # Atualizar interface
            self.auto_sim_button.config(text=" Simulação Automática")
            self.info_label.config(text="Simulação resetada! Pronta para iniciar.")

            # Redesenhar grafo limpo
            self.draw_graph()

            messagebox.showinfo(
                "Reset Completo", " Simulação automática resetada com sucesso!"
            )

    def _update_auto_simulation_loop(self):
        """Loop de atualização da simulação automática"""
        if not self.auto_simulation or not self.auto_simulation.is_running:
            return

        # Atualizar tempo simulado (como em animate_vehicle_step)
        current_real_time = time.time()
        if not hasattr(self, "auto_sim_last_time") or self.auto_sim_last_time == 0:
            self.auto_sim_last_time = current_real_time

        delta_time = current_real_time - self.auto_sim_last_time
        if self.traffic_manager:
            self.traffic_manager.update_simulation_time(delta_time)
            if self.auto_simulation.is_running:
                print(f"[AUTO_SIM] Hora: {self.traffic_manager.get_current_time_str()}")

        self.auto_sim_last_time = current_real_time

        # Atualizar simulação
        current_time = time.time()
        self.auto_simulation.update(current_time)

        # Atualizar visual
        self.draw_graph()

        # Atualizar estatísticas
        stats = self.auto_simulation.get_statistics()
        msg = f"Pedidos: {stats['total_requests']} | Completos: {stats['completed_trips']} | Ativos: {stats['active_vehicles']}/{stats['total_vehicles']} | Ocupação: {stats['occupancy_rate']:.1f}%"
        self.info_label.config(text=msg)

        # Continuar loop mais rápido para animação suave
        self.root.after(
            500, self._update_auto_simulation_loop
        )  # 500ms para animação mais suave

    def _show_auto_simulation_report(self):
        """Mostra relatório completo da simulação automática"""
        if not self.auto_simulation:
            return

        stats = self.auto_simulation.get_statistics()

        report_window = tk.Toplevel(self.root)
        report_window.title("Relatório de Simulação Automática - Frota Completa")
        report_window.geometry("900x700")

        # Gerar relatório
        report_lines = [
            "=" * 80,
            "  RELATÓRIO DE SIMULAÇÃO AUTOMÁTICA - FROTA COMPLETA",
            "=" * 80,
            "",
            f" Tempo de simulação: {stats['simulation_time_minutes']:.2f} minutos",
            f" Total de veículos: {stats['total_vehicles']}",
            "",
            "=" * 80,
            " TEMPO DE SIMULAÇÃO",
            "=" * 80,
            "",
            f" Hora inicial: {self.traffic_manager.get_start_time_str()}",
            f" Hora final: {self.traffic_manager.get_current_time_str()}",
            f" Duração simulada: {self.traffic_manager.get_simulation_duration_str()}",
            "",
            "=" * 80,
            "  PEDIDOS DE CLIENTES",
            "=" * 80,
            "",
            f" Total de pedidos recebidos: {stats['total_requests']}",
            f" Viagens completadas: {stats['completed_trips']}",
            f" Pedidos rejeitados: {stats['rejected_requests']}",
            f" Taxa de aceitação: {stats['acceptance_rate']:.1f}%",
            "",
            "=" * 80,
            "  DESEMPENHO DA FROTA",
            "=" * 80,
            "",
            f" Taxa de ocupação da frota: {stats['occupancy_rate']:.1f}%",
            f"   (Veículos ativos no momento: {stats['active_vehicles']}/{stats['total_vehicles']})",
            "",
            f"  Tempo médio de resposta ao pedido: {stats['avg_wait_time_minutes']:.2f} minutos",
            "   (Tempo desde o pedido até o pickup)",
            "",
            f" Tempo médio de viagem: {stats['avg_trip_time_minutes']:.2f} minutos",
            "   (Tempo desde pickup até dropoff)",
            "",
            "=" * 80,
            "  DETALHES DOS CLIENTES",
            "=" * 80,
            "",
        ]

        # Adicionar detalhes dos clientes
        for customer in stats["customers"]:
            status = " Completado" if customer.is_completed() else " Em andamento"
            wait_time = customer.get_wait_time(time.time())
            trip_time = customer.get_total_trip_time() or 0

            report_lines.extend(
                [
                    f"\n{customer.id}: {customer.pickup_location} → {customer.dropoff_location}",
                    f"  Status: {status}",
                    f"  Preferência: {customer.preferences.environmental_preference}",
                    f"  Veículo: {customer.assigned_vehicle_id or 'Nenhum'}",
                    f"  Tempo de espera: {wait_time:.2f} min",
                    f"  Tempo de viagem: {trip_time:.2f} min"
                    if customer.is_completed()
                    else "  Tempo de viagem: Em andamento",
                ]
            )

        report_lines.extend(
            [
                "",
                "=" * 80,
                f"Relatório gerado em: {datetime.now().strftime('%d/%m/%Y %H:%M:%S')}",
                "=" * 80,
            ]
        )

        report_text = "\n".join(report_lines)

        # Exibir relatório
        text_widget = scrolledtext.ScrolledText(
            report_window, font=("Courier", 9), bg="#1e1e1e", fg="#00ff00", wrap=tk.WORD
        )
        text_widget.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        text_widget.insert(tk.END, report_text)
        text_widget.config(state=tk.DISABLED)

    def setup_ui(self):
        """Cria layout 100% responsivo:
        - Topo: Mapa (expandível, 100% width)
        - Meio: Separador
        - Base: Painel de controles (100% width, altura fixa)
        """

        # === FRAME PRINCIPAL ===
        main_frame = ttk.Frame(self.root)
        main_frame.pack(fill=tk.BOTH, expand=True)
        main_frame.rowconfigure(0, weight=1)  # Mapa expande
        main_frame.rowconfigure(1, weight=0)  # Separador não expande
        main_frame.rowconfigure(2, weight=0)  # Controles não expandem
        main_frame.columnconfigure(0, weight=1)  # 100% width

        # === TOPO: MAPA (100% LARGURA, EXPANDÍVEL) ===
        map_frame = ttk.LabelFrame(
            main_frame, text=" Mapa da Cidade - Clique em nó para destino"
        )
        map_frame.grid(row=0, column=0, sticky="nsew", padx=5, pady=5)
        map_frame.rowconfigure(0, weight=1)
        map_frame.columnconfigure(0, weight=1)

        self.canvas = tk.Canvas(
            map_frame,
            bg="white",
            highlightthickness=1,
            highlightbackground="gray",
            cursor="crosshair",
        )
        self.canvas.grid(row=0, column=0, sticky="nsew")
        self.canvas.bind("<Button-1>", self.on_canvas_click)
        self.canvas.bind("<Configure>", self.on_canvas_resize)

        # === MEIO: SEPARADOR ===
        separator = ttk.Separator(main_frame, orient="horizontal")
        separator.grid(row=1, column=0, sticky="ew", pady=5)

        # === BASE: PAINEL DE CONTROLES (100% LARGURA, ALTURA FIXA) ===
        control_frame = ttk.LabelFrame(
            main_frame, text=" Controles - Selecione Carro e Destino"
        )
        control_frame.grid(row=2, column=0, sticky="ew", padx=5, pady=5)
        control_frame.columnconfigure(0, weight=0)
        control_frame.columnconfigure(1, weight=0)
        control_frame.columnconfigure(2, weight=0)
        control_frame.columnconfigure(3, weight=0)
        control_frame.columnconfigure(4, weight=0)
        control_frame.columnconfigure(5, weight=0)
        control_frame.columnconfigure(6, weight=1)  # Spacer para esticar à direita

        # --- ROW 1: SELEÇÕES ---

        # Selecionar Dataset/Grafo
        ttk.Label(control_frame, text=" Grafo:", font=("Arial", 10, "bold")).grid(
            row=0, column=0, padx=5, pady=5, sticky="w"
        )
        dataset_combo = ttk.Combobox(
            control_frame,
            textvariable=self.current_dataset,
            state="readonly",
            width=20,
            values=self.get_available_datasets(),
        )
        dataset_combo.grid(row=0, column=1, padx=5, pady=5, sticky="w")
        dataset_combo.bind("<<ComboboxSelected>>", self.on_dataset_changed)
        if self.get_available_datasets():
            dataset_combo.current(0)
            self.current_dataset.set(self.get_available_datasets()[0])

        # Selecionar Carro
        ttk.Label(control_frame, text=" Carro:", font=("Arial", 10, "bold")).grid(
            row=0, column=2, padx=5, pady=5, sticky="w"
        )
        self.vehicle_combo = ttk.Combobox(
            control_frame,
            textvariable=self.selected_vehicle,
            state="readonly",
            width=15,
            values=self.get_vehicle_list(),
        )
        self.vehicle_combo.grid(row=0, column=3, padx=5, pady=5, sticky="w")
        if self.get_vehicle_list():
            self.vehicle_combo.current(0)

        # Selecionar Destino
        ttk.Label(control_frame, text=" Destino:", font=("Arial", 10, "bold")).grid(
            row=0, column=4, padx=5, pady=5, sticky="w"
        )
        self.destination_combo = ttk.Combobox(
            control_frame,
            textvariable=self.selected_destination,
            state="readonly",
            width=15,
            values=self.get_node_list(),
        )
        self.destination_combo.grid(row=0, column=5, padx=5, pady=5, sticky="w")
        if self.get_node_list():
            self.destination_combo.current(0)

        # --- ROW 2: BOTÕES ---

        ttk.Button(
            control_frame, text=" Reproduzir", command=self.start_animation
        ).grid(row=1, column=0, padx=5, pady=5, sticky="ew")

        ttk.Button(control_frame, text=" Pausar", command=self.pause_animation).grid(
            row=1, column=1, padx=5, pady=5, sticky="ew"
        )

        ttk.Button(control_frame, text=" Resetar", command=self.reset_animation).grid(
            row=1, column=2, padx=5, pady=5, sticky="ew"
        )

        # Botão de simulação automática
        self.auto_sim_button = ttk.Button(
            control_frame,
            text=" Simulação Automática",
            command=self.toggle_auto_simulation,
        )
        self.auto_sim_button.grid(row=1, column=3, padx=5, pady=5, sticky="ew")

        # Botão de reset da simulação automática
        self.reset_auto_sim_button = ttk.Button(
            control_frame, text=" Reset Auto Sim", command=self.reset_auto_simulation
        )
        self.reset_auto_sim_button.grid(row=1, column=4, padx=5, pady=5, sticky="ew")

        # Info label
        self.info_label = ttk.Label(
            control_frame,
            text="Dica: Clique num nó no mapa para escolher destino, depois clique 'Reproduzir'",
            font=("Arial", 9),
            foreground="blue",
        )
        self.info_label.grid(row=2, column=0, columnspan=5, padx=5, pady=5, sticky="w")

        # Desenhar mapa inicial
        self.root.after(100, self.draw_graph)

        # Inicializar condições de trânsito se ativo
        if self.traffic_manager and self.show_traffic_conditions.get():
            self.root.after(200, self._initialize_traffic)

        # Iniciar loop de atualização de trânsito
        if self.traffic_manager:
            self.root.after(30000, self._update_traffic_loop)  # 30 segundos = 30000ms

    def get_vehicle_list(self):
        """Retorna lista de IDs de carros"""
        try:
            return [v.id for v in self.state.vehicles]
        except:
            return []

    def get_node_list(self):
        """Retorna lista de IDs de nós"""
        try:
            return [n.id for n in self.state.graph.nodes.values()]
        except:
            return []

    def get_available_datasets(self):
        """Retorna lista de datasets disponíveis"""
        from os import path, listdir

        data_dir = path.join(path.dirname(__file__), "..", "..", "data")
        datasets = []

        if path.exists(data_dir):
            for file in listdir(data_dir):
                if file.endswith(".json"):
                    # Nome amigável (remover .json e braga_)
                    display_name = (
                        file.replace(".json", "")
                        .replace("braga_", "")
                        .replace("_", " ")
                        .title()
                    )
                    datasets.append((display_name, file))

        # Retornar apenas os nomes para exibição
        return [name for name, _ in datasets] if datasets else ["Dataset Atual"]

    def get_dataset_filename(self, display_name):
        """Converte nome de exibição para nome de arquivo"""
        from os import path, listdir

        data_dir = path.join(path.dirname(__file__), "..", "..", "data")

        if path.exists(data_dir):
            for file in listdir(data_dir):
                if file.endswith(".json"):
                    name = (
                        file.replace(".json", "")
                        .replace("braga_", "")
                        .replace("_", " ")
                        .title()
                    )
                    if name == display_name:
                        return file

        return "braga_data.json"  # Fallback

    def on_dataset_changed(self, event=None):
        """Callback quando o dataset é alterado"""
        selected = self.current_dataset.get()

        if messagebox.askyesno(
            "Mudar Grafo",
            f"Deseja carregar o grafo '{selected}'?\n\nA simulação será reiniciada.",
        ):
            self.load_new_dataset(selected)

    def load_new_dataset(self, display_name):
        """Carrega novo dataset"""
        from load_dataset import load_dataset
        from os import path

        filename = self.get_dataset_filename(display_name)
        data_path = path.join(path.dirname(__file__), "..", "..", "data", filename)

        try:
            # Carregar novo dataset
            new_state = load_dataset(data_path)

            # Substituir estado
            self.state = new_state
            self.engine.state = new_state

            # Reset animação
            self.animating = False
            self.vehicle_in_animation = None
            self.animation_path = []

            # Limpar cache de posições
            self.scaled_positions = {}

            # Atualizar combos
            self.vehicle_combo["values"] = self.get_vehicle_list()
            if self.get_vehicle_list():
                self.vehicle_combo.current(0)

            self.destination_combo["values"] = self.get_node_list()
            if self.get_node_list():
                self.destination_combo.current(0)

            # Redesenhar
            self.draw_graph()

            self.info_label.config(
                text=f"✓ Grafo '{display_name}' carregado! Nós: {len(self.state.graph.nodes)}, Veículos: {len(self.state.vehicles)}"
            )

        except Exception as e:
            messagebox.showerror("Erro", f"Erro ao carregar dataset:\n{e}")
            self.info_label.config(text=f"❌ Erro ao carregar '{display_name}'")

    def on_canvas_resize(self, event):
        """Evento quando canvas é redimensionado"""
        self.canvas_width = event.width
        self.canvas_height = event.height
        self.scaled_positions = {}  # Invalidar cache
        self.draw_graph()

    def on_canvas_click(self, event):
        """Evento quando clica no canvas - selecionar nó como destino"""
        if not self.state.graph.nodes:
            return

        # Encontrar nó mais próximo ao clique
        click_x, click_y = event.x, event.y
        closest_node = None
        closest_dist = float("inf")

        for node_id, node in self.state.graph.nodes.items():
            if node_id not in self.scaled_positions:
                continue

            node_x, node_y = self.scaled_positions[node_id]
            dist = ((click_x - node_x) ** 2 + (click_y - node_y) ** 2) ** 0.5

            if dist < 30 and dist < closest_dist:  # Raio de 30px
                closest_dist = dist
                closest_node = node_id

        if closest_node:
            self.selected_destination.set(closest_node)
            self.info_label.config(text=f"✓ Destino selecionado: {closest_node}")
            self.draw_graph()  # Realçar nó selecionado

    def scale_position(self, node):
        """Escala posição real do nó para coordenadas do canvas"""
        if not self.state.graph.nodes:
            return 0, 0

        # Cache para evitar recálculo
        if node.id in self.scaled_positions:
            return self.scaled_positions[node.id]

        # Encontrar bounds
        all_nodes = list(self.state.graph.nodes.values())
        min_lon = min(n.position.longitude for n in all_nodes)
        max_lon = max(n.position.longitude for n in all_nodes)
        min_lat = min(n.position.latitude for n in all_nodes)
        max_lat = max(n.position.latitude for n in all_nodes)

        # Margem de 50px em cada lado
        margin = 50
        available_width = self.canvas_width - 2 * margin
        available_height = self.canvas_height - 2 * margin

        # Escalar com proporção
        lon_range = max_lon - min_lon or 0.0001
        lat_range = max_lat - min_lat or 0.0001

        x = margin + (node.position.longitude - min_lon) / lon_range * available_width
        y = margin + (node.position.latitude - min_lat) / lat_range * available_height

        self.scaled_positions[node.id] = (x, y)
        return x, y

    def find_path_bfs(self, start_node_id, end_node_id):
        """BFS para encontrar caminho entre dois nós
        Retorna: [start_node_id, node2, node3, ..., end_node_id]
        """
        if start_node_id == end_node_id:
            return [start_node_id]

        graph = self.state.graph
        queue = deque([(start_node_id, [start_node_id])])
        visited = {start_node_id}

        while queue:
            current, path = queue.popleft()

            # Procurar arestas saindo deste nó
            for edge_id, edge in graph.edges.items():
                if edge.from_node == current and not edge.is_blocked:
                    neighbor = edge.to_node

                    if neighbor == end_node_id:
                        return path + [neighbor]

                    if neighbor not in visited:
                        visited.add(neighbor)
                        queue.append((neighbor, path + [neighbor]))

        # Sem caminho encontrado
        return []

    def find_path_with_algorithm(self, start_node_id, end_node_id, vehicle):
        """Encontra caminho usando o algoritmo selecionado
        Retorna: [start_node_id, node2, node3, ..., end_node_id]
        """
        if start_node_id == end_node_id:
            return [start_node_id]

        # Se o algoritmo for A*, usar com critério de otimização
        if self.algorithm == "A_STAR" and a_star_find_path:
            try:
                path, cost = a_star_find_path(
                    self.state.graph,
                    start_node_id,
                    end_node_id,
                    vehicle,
                    optimization_criterion=self.optimization_criterion,
                )
                return path if path else []
            except TypeError:
                # Se a versão do a_star_find_path não suporta optimization_criterion
                path, cost = a_star_find_path(
                    self.state.graph, start_node_id, end_node_id, vehicle
                )
                return path if path else []

        # Se o algoritmo for Greedy, usar com critério de otimização
        elif self.algorithm == "GREEDY" and greedy_find_path:
            try:
                path, cost = greedy_find_path(
                    self.state.graph,
                    start_node_id,
                    end_node_id,
                    vehicle,
                    optimization_criterion=self.optimization_criterion,
                )
                return path if path else []
            except Exception as e:
                print(f" Erro ao usar Greedy: {e}")
                # Fallback para BFS
                return self.find_path_bfs(start_node_id, end_node_id)

        # Para outros algoritmos, usar BFS como fallback
        return self.find_path_bfs(start_node_id, end_node_id)

    def start_animation(self):
        """Inicia animação de carro nó-a-nó"""
        vehicle_id = self.selected_vehicle.get()
        destination_id = self.selected_destination.get()

        if not vehicle_id:
            messagebox.showwarning("Aviso", "Selecione um carro!")
            return

        if not destination_id:
            messagebox.showwarning("Aviso", "Selecione um destino!")
            return

        # Encontrar veículo
        vehicle = None
        for v in self.state.vehicles:
            if v.id == vehicle_id:
                vehicle = v
                break

        if not vehicle:
            messagebox.showerror("Erro", f"Carro {vehicle_id} não encontrado!")
            return

        # Não bloquear viagem mesmo com combustível baixo
        # O veículo pode fazer a viagem e reabastecer durante o trajeto se passar por uma estação
        self.target_station = None

        # Encontrar caminho
        current_node = (
            vehicle.position.node_id if hasattr(vehicle.position, "node_id") else None
        )

        # Se veículo não tem node_id, usar o nó mais próximo
        if not current_node and self.state.graph.nodes:
            # Encontrar nó mais próximo
            min_dist = float("inf")
            for node_id, node in self.state.graph.nodes.items():
                dist = (
                    (vehicle.position.longitude - node.position.longitude) ** 2
                    + (vehicle.position.latitude - node.position.latitude) ** 2
                ) ** 0.5
                if dist < min_dist:
                    min_dist = dist
                    current_node = node_id

        if not current_node:
            messagebox.showerror(
                "Erro", "Não foi possível determinar posição do carro!"
            )
            return

        # Encontrar caminho usando o algoritmo selecionado
        path = self.find_path_with_algorithm(current_node, destination_id, vehicle)

        if not path:
            messagebox.showerror(
                "Erro", f"Sem caminho entre {current_node} e {destination_id}!"
            )
            return

        # Iniciar animação
        self.animation_path = path
        self.vehicle_in_animation = vehicle
        self.animating = True

        # Calcular tempo total da viagem (em minutos)
        self.journey_total_time_minutes = 0
        for i in range(len(path) - 1):
            from_id = path[i]
            to_id = path[i + 1]
            edge = self.state.graph.get_edge(from_id, to_id)
            if edge:
                # Tempo da aresta
                edge_time = edge.time_minutes
                # Aplicar multiplicador de tráfico se disponível
                traffic_mult = (
                    self.traffic_manager.get_traffic_multiplier(from_id, to_id)
                    if self.traffic_manager
                    else 1.0
                )
                self.journey_total_time_minutes += edge_time * traffic_mult

        # Rastrear tempo decorrido na viagem (em minutos)
        self.journey_elapsed_time_minutes = 0
        # Registar hora inicial
        self.journey_start_time = (
            self.traffic_manager.get_current_time_str()
            if self.traffic_manager
            else "06:00"
        )
        self.animation_start_time = time.time()
        self.last_animation_time = (
            0  # Será inicializado na primeira chamada de animate_vehicle_step()
        )
        self.refueling_start_time = None  # Tempo de início do reabastecimento
        self.info_label.config(
            text=f"Reproduzindo {vehicle_id}: {' → '.join(path)} ({len(path)} nós) | Combustível: {vehicle.fuel_percentage():.1f}%"
        )

        # Se o caminho tem apenas 1 nó (carro já está no destino), mostrar relatório direto
        if len(path) < 2:
            self.animating = False
            final_node_id = path[0]
            final_node = self.state.graph.nodes[final_node_id]

            # Verificar se estava indo para uma estação
            if self.target_station:
                # Verificar se a estação está disponível
                if not self.target_station.is_available():
                    self.info_label.config(
                        text=f"Estação {self.target_station.name} ocupada! Aguardando..."
                    )
                    # Tentar novamente após 1 segundo
                    self.root.after(1000, lambda: self.animate_path(vehicle_id, path))
                    return

                # Ocupar a estação
                self.target_station.add_vehicle_to_queue(vehicle.id)
                vehicle.start_refueling()
                self.info_label.config(
                    text=f"{vehicle_id} está reabastecendo em {self.target_station.name}..."
                )
                self.root.after(
                    3000, lambda: self.complete_refueling(vehicle, self.target_station)
                )
                return

            self.vehicle_in_animation.position.longitude = final_node.position.longitude
            self.vehicle_in_animation.position.latitude = final_node.position.latitude
            if hasattr(self.vehicle_in_animation.position, "node_id"):
                self.vehicle_in_animation.position.node_id = final_node_id
            self.info_label.config(
                text=f"Reprodução completa! {self.vehicle_in_animation.id} está em {final_node_id}"
            )
            self.draw_graph()
            # Gerar relatório automaticamente
            self.root.after(500, self._show_report)
        else:
            # Animar nó-a-nó
            self.animate_vehicle_step()

    def animate_vehicle_step(self):
        """Um passo da animação do veículo - usa tempo baseado em grafo"""
        if not self.animating or not self.vehicle_in_animation:
            return

        print(f"[ANIM] Step | {self.vehicle_in_animation.id}")

        # Usar tempo real para passar para simulação (1 segundo real = 1 minuto simulado)
        current_real_time = time.time()
        if not hasattr(self, "last_animation_time") or self.last_animation_time == 0:
            self.last_animation_time = current_real_time

        delta_real_seconds = current_real_time - self.last_animation_time
        delta_graph_minutes = (
            delta_real_seconds * 1.0
        )  # time_speed = 1.0 (1 seg real = 1 min simulado)

        # Avançar tempo da viagem (em minutos)
        self.journey_elapsed_time_minutes += delta_graph_minutes

        # Atualizar relógio com tempo da viagem
        if self.traffic_manager and delta_graph_minutes > 0:
            self.traffic_manager.add_travel_time(delta_graph_minutes)
            log_msg = f"[animate_vehicle_step] Elapsed: {self.journey_elapsed_time_minutes:.2f}min / {self.journey_total_time_minutes:.2f}min | Hora: {self.traffic_manager.get_current_time_str()}"
            with open("/tmp/taxigreen_debug.log", "a") as f:
                f.write(log_msg + "\n")

        self.last_animation_time = current_real_time

        # Redesenhar para atualizar o relógio
        self.draw_graph()

        # Verificar se viagem completada
        if self.journey_elapsed_time_minutes >= self.journey_total_time_minutes:
            # Viagem completa
            self.animating = False
            final_node_id = self.animation_path[-1]
            final_node = self.state.graph.nodes[final_node_id]

            # Consumir combustível do último trecho
            if len(self.animation_path) > 1:
                prev_node_id = self.animation_path[-2]
                edge = self.state.graph.get_edge(prev_node_id, final_node_id)
                if edge:
                    self.vehicle_in_animation.travel(
                        edge.distance_km, with_passengers=False
                    )

            self.vehicle_in_animation.position.longitude = final_node.position.longitude
            self.vehicle_in_animation.position.latitude = final_node.position.latitude
            if hasattr(self.vehicle_in_animation.position, "node_id"):
                self.vehicle_in_animation.position.node_id = final_node_id

            # Verificar se chegou perto de uma estação (se target_station está definido)
            if self.target_station:
                # Verificar se a estação está disponível
                if not self.target_station.is_available():
                    self.info_label.config(
                        text=f"Estação {self.target_station.name} ocupada! Aguardando..."
                    )
                    # Tentar novamente após 1 segundo
                    self.root.after(1000, lambda: self.animate_vehicle_step())
                    return

                # Ocupar a estação
                self.target_station.add_vehicle_to_queue(self.vehicle_in_animation.id)
                self.vehicle_in_animation.start_refueling()
                self.info_label.config(
                    text=f"{self.vehicle_in_animation.id} está reabastecendo em {self.target_station.name}..."
                )
                self.draw_graph()
                # Aguardar 3 segundos e completar reabastecimento
                station = self.target_station
                self.root.after(
                    3000,
                    lambda: self.complete_refueling(self.vehicle_in_animation, station),
                )
                return

            self.info_label.config(
                text=f"Viagem completa! {self.vehicle_in_animation.id} chegou a {final_node_id} | Combustível: {self.vehicle_in_animation.fuel_percentage():.1f}%"
            )
            self.draw_graph()

            # Gerar relatório automaticamente
            self.root.after(500, self._show_report)
            return

        # Viagem em progresso - interpolar posição baseada em progresso
        progress_ratio = (
            self.journey_elapsed_time_minutes / self.journey_total_time_minutes
        )

        # Encontrar qual aresta estamos neste momento
        accumulated_time = 0
        for i in range(len(self.animation_path) - 1):
            from_id = self.animation_path[i]
            to_id = self.animation_path[i + 1]
            edge = self.state.graph.get_edge(from_id, to_id)
            if edge:
                edge_time = edge.time_minutes
                traffic_mult = (
                    self.traffic_manager.get_traffic_multiplier(from_id, to_id)
                    if self.traffic_manager
                    else 1.0
                )
                edge_total_time = edge_time * traffic_mult

                if (
                    accumulated_time + edge_total_time
                    >= self.journey_elapsed_time_minutes
                ):
                    # Estamos nesta aresta
                    from_node = self.state.graph.nodes[from_id]
                    to_node = self.state.graph.nodes[to_id]

                    # Progresso nesta aresta
                    time_in_edge = self.journey_elapsed_time_minutes - accumulated_time
                    edge_progress = time_in_edge / edge_total_time
                    edge_progress = min(1.0, max(0.0, edge_progress))

                    # Interpolar posição
                    self.vehicle_in_animation.position.latitude = (
                        from_node.position.latitude
                        + (to_node.position.latitude - from_node.position.latitude)
                        * edge_progress
                    )
                    self.vehicle_in_animation.position.longitude = (
                        from_node.position.longitude
                        + (to_node.position.longitude - from_node.position.longitude)
                        * edge_progress
                    )

                    # Consumir combustível quando completa aresta
                    if edge_progress >= 0.99 and not hasattr(
                        self, f"_consumed_{from_id}_{to_id}"
                    ):
                        self.vehicle_in_animation.travel(
                            edge.distance_km, with_passengers=False
                        )
                        setattr(self, f"_consumed_{from_id}_{to_id}", True)

                        # Verificar se há uma estação próxima
                        if self.vehicle_in_animation.needs_refuel():
                            nearby_station = self.find_station_near_node(
                                to_node, self.vehicle_in_animation
                            )
                            if nearby_station:
                                if nearby_station.is_available():
                                    # Parar para reabastecer
                                    self.animating = False
                                    self.target_station = nearby_station
                                    self.info_label.config(
                                        text=f" {self.vehicle_in_animation.id} vai reabastecer em {nearby_station.name}... Combustível: {self.vehicle_in_animation.fuel_percentage():.1f}%"
                                    )
                                    self.root.after(500, self._show_refuel_dialog)
                                    return

                    break

                accumulated_time += edge_total_time

        # Agendar próximo passo (50ms = 20 FPS)
        self.root.after(50, self.animate_vehicle_step)

    def complete_refueling(self, vehicle, station):
        """Completa o reabastecimento do veículo"""
        vehicle.refuel()
        # Liberar a estação
        station.remove_vehicle_from_queue(vehicle.id)
        self.info_label.config(
            text=f" {vehicle.id} reabastecido em {station.name}! Combustível: {vehicle.fuel_percentage():.1f}%"
        )
        self.target_station = None  # Limpar estação alvo
        self.draw_graph()
        # Gerar relatório
        self.root.after(500, self._show_report)

    def find_station_near_node(self, node, vehicle):
        """Encontra uma estação próxima de um nó específico"""
        station_type = (
            "charging_station"
            if vehicle.fuel_type.value == "electricity"
            else "refuel_station"
        )
        threshold_distance = 0.01  # Distância limite para considerar "próximo" (graus)

        for station in self.state.stations:
            if station.type == station_type and station.is_available():
                # Calcular distância euclidiana
                dist = (
                    (node.position.longitude - station.position.longitude) ** 2
                    + (node.position.latitude - station.position.latitude) ** 2
                ) ** 0.5

                if dist < threshold_distance:
                    return station

        return None

    def continue_after_refuel(self, vehicle, station):
        """Continua a viagem após reabastecer"""
        vehicle.refuel()
        # Liberar a estação
        station.remove_vehicle_from_queue(vehicle.id)

        # Ajustar o tempo de início da animação para compensar os 3 segundos de pausa
        # Assim a animação continua do ponto onde parou, sem acelerar
        if self.animation_pause_time > 0:
            pause_duration = time.time() - self.animation_pause_time
            self.animation_start_time += pause_duration
            self.animation_pause_time = 0

        # Atualizar tempo de rastreio para tempo simulado
        self.last_animation_time = time.time()

        self.info_label.config(
            text=f"{vehicle.id} reabastecido em {station.name} (até 90%)! Continuando viagem..."
        )
        self.animating = True
        self.draw_graph()
        # Retomar animação no mesmo ritmo (não acelerar)
        # O animate_vehicle_step já tem lógica para continuar do ponto atual
        self.animate_vehicle_step()

    def pause_animation(self):
        """Pausa animação"""
        if self.animating:
            self.animating = False
            self.info_label.config(text="Animação pausada")

    def reset_animation(self):
        """Reseta animação"""
        self.animating = False
        self.animation_path = []
        self.vehicle_in_animation = None
        self.info_label.config(text="Animação resetada")
        self.draw_graph()

    def draw_graph(self):
        """Desenha grafo completo no canvas"""
        self.canvas.delete("all")

        if not self.state.graph.nodes:
            return

        # Debug: verificar se trânsito está ativo
        traffic_active = (
            self.show_traffic_conditions.get() and self.traffic_manager is not None
        )
        if traffic_active:
            edges_with_traffic = 0

        # === DESENHAR ARESTAS ===
        for edge_id, edge in self.state.graph.edges.items():
            if (
                edge.from_node not in self.state.graph.nodes
                or edge.to_node not in self.state.graph.nodes
            ):
                continue

            from_node = self.state.graph.nodes[edge.from_node]
            to_node = self.state.graph.nodes[edge.to_node]

            x1, y1 = self.scale_position(from_node)
            x2, y2 = self.scale_position(to_node)

            # Cor baseada em condições de trânsito ou bloqueio
            edge_color = "gray"
            edge_width = 1
            traffic_condition_text = None

            if edge.is_blocked:
                edge_color = "red"
                edge_width = 2
            elif traffic_active:
                # Obter condição de trânsito
                condition_info = self.traffic_manager.get_condition_info(
                    edge.from_node, edge.to_node
                )
                edge_color = condition_info["color"]
                edge_width = (
                    5 if condition_info["emoji"] != "🟢" else 2
                )  # MUITO MAIS GROSSO

                # Guardar texto da condição para mostrar depois
                traffic_condition_text = (
                    f"{condition_info['emoji']} {condition_info['level']}"
                )

                # Debug: contar arestas com trânsito
                if condition_info["emoji"] != "🟢":
                    edges_with_traffic += 1

            self.canvas.create_line(
                x1, y1, x2, y2, fill=edge_color, width=edge_width, tags="edge"
            )

            if traffic_condition_text:
                mid_x = (x1 + x2) / 2
                mid_y = (y1 + y2) / 2

                # Fundo para o texto
                self.canvas.create_rectangle(
                    mid_x - 35,
                    mid_y - 10,
                    mid_x + 35,
                    mid_y + 10,
                    fill="white",
                    outline=edge_color,
                    width=2,
                )
                # Texto da condição
                self.canvas.create_text(
                    mid_x,
                    mid_y,
                    text=traffic_condition_text,
                    font=("Arial", 8, "bold"),
                    fill=edge_color,
                )

            current_dataset_name = self.current_dataset.get().lower()
            show_weights = "optimized" in current_dataset_name

            if show_weights:
                mid_x = (x1 + x2) / 2
                mid_y = (y1 + y2) / 2

                if "time" in current_dataset_name:
                    weight_text = f"{edge.time_minutes:.1f}m | {edge.distance_km:.1f}km"
                    weight_color = "darkblue"
                else:
                    # Grafo Distance Optimized - destacar distância
                    weight_text = f"{edge.distance_km:.1f}km | {edge.time_minutes:.1f}m"
                    weight_color = "darkgreen"

                # Desenhar fundo branco para legibilidade
                bbox = self.canvas.create_text(
                    mid_x,
                    mid_y,
                    text=weight_text,
                    font=("Arial", 8, "bold"),
                    fill=weight_color,
                )

                # Pegar bounding box do texto
                text_bbox = self.canvas.bbox(bbox)
                if text_bbox:
                    x0, y0, x1_text, y1_text = text_bbox
                    # Criar retângulo branco semi-transparente atrás do texto
                    self.canvas.create_rectangle(
                        x0 - 2,
                        y0 - 1,
                        x1_text + 2,
                        y1_text + 1,
                        fill="white",
                        outline="",
                        tags="weight_bg",
                    )
                    # Recriar texto em cima do fundo
                    self.canvas.create_text(
                        mid_x,
                        mid_y,
                        text=weight_text,
                        font=("Arial", 8, "bold"),
                        fill=weight_color,
                        tags="weight_text",
                    )
                    # Garantir que os pesos fiquem acima das linhas
                    self.canvas.tag_raise("weight_bg")
                    self.canvas.tag_raise("weight_text")

        # === DESENHAR CAMINHO DE ANIMAÇÃO (verde tracejado) ===
        if self.animating and self.animation_path:
            for i in range(len(self.animation_path) - 1):
                node_id1 = self.animation_path[i]
                node_id2 = self.animation_path[i + 1]

                node1 = self.state.graph.nodes[node_id1]
                node2 = self.state.graph.nodes[node_id2]

                x1, y1 = self.scale_position(node1)
                x2, y2 = self.scale_position(node2)

                self.canvas.create_line(
                    x1, y1, x2, y2, fill="lime", width=4, dash=(4, 2)
                )

        # === DESENHAR NÓS (LOCALIZAÇÕES NORMAIS) ===
        for node_id, node in self.state.graph.nodes.items():
            x, y = self.scale_position(node)

            # Realçar nó destino selecionado
            if node_id == self.selected_destination.get():
                self.canvas.create_oval(
                    x - 15,
                    y - 15,
                    x + 15,
                    y + 15,
                    fill="yellow",
                    outline="orange",
                    width=3,
                )

            # Nó normal (localização para buscar passageiros)
            self.canvas.create_oval(
                x - 8, y - 8, x + 8, y + 8, fill="lightblue", outline="blue", width=2
            )

            # ID do nó
            self.canvas.create_text(
                x, y + 20, text=node_id, font=("Arial", 7, "bold"), fill="black"
            )

        # === DESENHAR CAMINHOS DOS VEÍCULOS NA SIMULAÇÃO AUTOMÁTICA ===
        if self.auto_simulation and self.auto_simulation.is_running:
            for vehicle_id in self.auto_simulation.active_vehicles:
                path = self.auto_simulation.get_vehicle_path(vehicle_id)
                if path and len(path) > 1:
                    for i in range(len(path) - 1):
                        node_id1 = path[i]
                        node_id2 = path[i + 1]

                        if (
                            node_id1 in self.state.graph.nodes
                            and node_id2 in self.state.graph.nodes
                        ):
                            node1 = self.state.graph.nodes[node_id1]
                            node2 = self.state.graph.nodes[node_id2]

                            x1, y1 = self.scale_position(node1)
                            x2, y2 = self.scale_position(node2)

                            self.canvas.create_line(
                                x1, y1, x2, y2, fill="cyan", width=3, dash=(4, 2)
                            )

        # === DESENHAR CLIENTES AGUARDANDO ===
        if self.auto_simulation and self.auto_simulation.is_running:
            waiting_customers = self.auto_simulation.get_waiting_customers()
            for customer in waiting_customers:
                if customer.pickup_location in self.state.graph.nodes:
                    node = self.state.graph.nodes[customer.pickup_location]
                    x, y = self.scale_position(node)

                    # Desenhar cliente como pessoa aguardando - MAIOR e mais visível
                    # Círculo de fundo amarelo brilhante
                    self.canvas.create_oval(
                        x - 18,
                        y - 18,
                        x + 18,
                        y + 18,
                        fill="gold",
                        outline="orange",
                        width=4,
                    )

                    # Ícone de pessoa
                    self.canvas.create_text(
                        x, y, text="🧑", font=("Arial", 24), fill="black"
                    )

                    # ID do cliente acima
                    self.canvas.create_rectangle(
                        x - 25,
                        y - 35,
                        x + 25,
                        y - 22,
                        fill="orange",
                        outline="black",
                        width=2,
                    )
                    self.canvas.create_text(
                        x,
                        y - 28,
                        text=customer.id,
                        font=("Arial", 8, "bold"),
                        fill="white",
                    )

                    # Preferência do cliente abaixo
                    pref_icon = (
                        "⚡"
                        if customer.preferences.environmental_preference == "electric"
                        else "💰"
                        if customer.preferences.environmental_preference == "low_cost"
                        else "🚗"
                    )
                    self.canvas.create_text(
                        x, y + 25, text=pref_icon, font=("Arial", 14), fill="black"
                    )

            # Desenhar clientes sendo transportados
            in_transit = self.auto_simulation.get_customers_in_transit()
            for customer in in_transit:
                # Encontrar o veículo que está transportando
                for vehicle_id in self.auto_simulation.active_vehicles:
                    if (
                        self.auto_simulation.get_vehicle_customer(vehicle_id)
                        == customer
                    ):
                        # Desenhar ícone pequeno ao lado do veículo
                        vehicle = None
                        for v in self.state.vehicles:
                            if v.id == vehicle_id:
                                vehicle = v
                                break

                        if vehicle:
                            vx, vy = self.scale_position_vehicle(vehicle)
                            # Ícone pequeno de pessoa no veículo
                            self.canvas.create_text(
                                vx + 15,
                                vy - 15,
                                text="👤",
                                font=("Arial", 12),
                                fill="cyan",
                            )
                        break

        # === DESENHAR ESTAÇÕES (SEPARADAS DOS NÓS) ===
        for station in self.state.stations:
            x, y = self.scale_position_station(station)

            # Verificar se a estação está ocupada
            is_occupied = not station.is_available()

            # Distinguir estações de recarga e postos de combustível
            if station.type == "charging_station":
                # Estação de recarga (elétrica) - verde ou cinza se ocupada
                fill_color = "lightgray" if is_occupied else "lightgreen"
                outline_color = "darkgray" if is_occupied else "green"
                self.canvas.create_rectangle(
                    x - 10,
                    y - 10,
                    x + 10,
                    y + 10,
                    fill=fill_color,
                    outline=outline_color,
                    width=3,
                )
                self.canvas.create_text(
                    x, y - 18, text="⚡", font=("Arial", 16), fill=outline_color
                )

                # Adicionar "X" vermelho se ocupada
                if is_occupied:
                    self.canvas.create_text(
                        x, y, text="✖", font=("Arial", 16, "bold"), fill="red"
                    )

            elif station.type == "refuel_station":
                # Posto de abastecimento (combustível) - vermelho ou cinza se ocupada
                fill_color = "lightgray" if is_occupied else "lightcoral"
                outline_color = "darkgray" if is_occupied else "red"
                self.canvas.create_rectangle(
                    x - 10,
                    y - 10,
                    x + 10,
                    y + 10,
                    fill=fill_color,
                    outline=outline_color,
                    width=3,
                )
                self.canvas.create_text(
                    x, y - 18, text="⛽", font=("Arial", 16), fill=outline_color
                )

                # Adicionar "X" vermelho se ocupada
                if is_occupied:
                    self.canvas.create_text(
                        x, y, text="✖", font=("Arial", 16, "bold"), fill="red"
                    )

            # ID da estação
            self.canvas.create_text(
                x, y + 20, text=station.id, font=("Arial", 7, "bold"), fill="black"
            )

        # === DESENHAR VEÍCULOS ===
        for vehicle in self.state.vehicles:
            x, y = self.scale_position_vehicle(vehicle)

            # Verificar se veículo está ativo na simulação automática
            is_auto_sim_active = (
                self.auto_simulation
                and self.auto_simulation.is_running
                and vehicle.id in self.auto_simulation.active_vehicles
            )

            # Realçar veículo em animação
            if vehicle == self.vehicle_in_animation:
                # Pulso ao redor
                pulse = 5 + 3 * abs(sin(time.time() * 3))
                self.canvas.create_oval(
                    x - pulse - 12,
                    y - pulse - 12,
                    x + pulse + 12,
                    y + pulse + 12,
                    fill="",
                    outline="orange",
                    width=2,
                )

                # Halo
                self.canvas.create_oval(
                    x - 20,
                    y - 20,
                    x + 20,
                    y + 20,
                    fill="",
                    outline="yellow",
                    width=1,
                    dash=(2, 2),
                )

            # Realçar veículo ativo na simulação automática
            elif is_auto_sim_active:
                self.canvas.create_oval(
                    x - 15, y - 15, x + 15, y + 15, fill="", outline="cyan", width=2
                )

            # Desenhar veículo (com ícone se disponível)
            vehicle_type = getattr(vehicle, "vehicle_type", "truck")

            # Cor baseada no tipo de combustível e nível
            if vehicle.needs_refuel():
                vehicle_color = "red"  # Baixo combustível
            elif vehicle.fuel_type.value == "electricity":
                vehicle_color = "blue"
            else:
                vehicle_color = "green"

            if vehicle == self.vehicle_in_animation:
                vehicle_color = "orange"
            elif is_auto_sim_active:
                vehicle_color = "cyan"  # Azul claro para veículos ativos

            if vehicle_type in self.vehicle_images:
                # Usar ícone
                try:
                    self.canvas.create_image(
                        x, y, image=self.vehicle_images[vehicle_type]
                    )
                except:
                    # Fallback para círculo
                    self.canvas.create_oval(
                        x - 8,
                        y - 8,
                        x + 8,
                        y + 8,
                        fill=vehicle_color,
                        outline="darkgreen",
                        width=2,
                    )
            else:
                # Círculo com cor
                self.canvas.create_oval(
                    x - 8,
                    y - 8,
                    x + 8,
                    y + 8,
                    fill=vehicle_color,
                    outline="darkgreen",
                    width=2,
                )

            # Desenhar ID do veículo com background e status de combustível
            fuel_pct = vehicle.fuel_percentage()
            fuel_text = f"{vehicle.id}\n⛽{fuel_pct:.0f}%"

            # Background para o texto (retângulo branco com contorno)
            # Cor do fundo baseada no nível de combustível
            if vehicle.needs_refuel():
                bg_color = "#ffcccc"  # Vermelho claro
            else:
                bg_color = "white"

            self.canvas.create_rectangle(
                x - 50, y + 12, x + 50, y + 45, fill=bg_color, outline="black", width=2
            )

            # Texto com ID e combustível (MUITO MAIOR)
            self.canvas.create_text(
                x,
                y + 28,
                text=fuel_text,
                font=("Arial", 11, "bold"),
                fill="black",
                width=100,
            )

        # === RELÓGIO E INFORMAÇÕES DE TRÂNSITO (CANTO SUPERIOR ESQUERDO) ===
        if self.show_traffic_conditions.get() and self.traffic_manager:
            clock_x = 20
            clock_y = 20

            # Obter informações do traffic manager
            current_time = self.traffic_manager.get_current_time_str()
            period_name, traffic_level = self.traffic_manager._get_hour_period()

            # Debug: mostrar que estamos desenhando
            if self.animating:
                print(f"[RELÓGIO] Desenhando: {current_time}")

            # Fundo do relógio
            self.canvas.create_rectangle(
                clock_x - 10,
                clock_y - 10,
                clock_x + 180,
                clock_y + 90,
                fill="white",
                outline="black",
                width=2,
            )

            # Ícone e hora
            self.canvas.create_text(
                clock_x + 80,
                clock_y + 10,
                text=f"🕐 {current_time}",
                font=("Arial", 20, "bold"),
                fill="darkblue",
            )

            # Período do dia e nível de tráfego
            period_color = {"high": "red", "medium": "orange", "low": "green"}.get(
                traffic_level, "black"
            )

            period_emoji = {"high": "🚨", "medium": "⚠️", "low": "✅"}.get(
                traffic_level, "•"
            )

            self.canvas.create_text(
                clock_x + 80,
                clock_y + 45,
                text=f"{period_emoji} {period_name}",
                font=("Arial", 10, "bold"),
                fill=period_color,
            )

            # Dica do período
            period_tip = {
                "high": "Tráfego pesado esperado",
                "medium": "Tráfego moderado",
                "low": "Tráfego livre",
            }.get(traffic_level, "")

            self.canvas.create_text(
                clock_x + 80,
                clock_y + 65,
                text=period_tip,
                font=("Arial", 8, "italic"),
                fill="gray",
            )

        # === LEGENDA DOS PESOS (SE GRAFOS OTIMIZADOS) ===
        current_dataset_name = self.current_dataset.get().lower()
        if "optimized" in current_dataset_name:
            # Posição da legenda (canto superior direito)
            legend_x = self.canvas_width - 180
            legend_y = 20

            # Fundo da legenda
            self.canvas.create_rectangle(
                legend_x - 10,
                legend_y - 10,
                legend_x + 170,
                legend_y + 80,
                fill="white",
                outline="black",
                width=2,
            )

            # Título da legenda
            self.canvas.create_text(
                legend_x + 80,
                legend_y + 5,
                text="📊 Pesos nas Arestas",
                font=("Arial", 9, "bold"),
                fill="black",
            )

            # Explicação - mostra ambos mas destaca o principal
            if "time" in current_dataset_name:
                # Time Optimized - destaca tempo
                self.canvas.create_text(
                    legend_x + 80,
                    legend_y + 25,
                    text="⏱️ Tempo | Distância",
                    font=("Arial", 8, "bold"),
                    fill="darkblue",
                )
                self.canvas.create_text(
                    legend_x + 80,
                    legend_y + 45,
                    text="Grafo otimizado para tempo",
                    font=("Arial", 7),
                    fill="darkblue",
                )
                self.canvas.create_text(
                    legend_x + 80,
                    legend_y + 62,
                    text="(mostra ambos, destaca tempo)",
                    font=("Arial", 6, "italic"),
                    fill="gray",
                )
            else:
                # Distance Optimized - destaca distância
                self.canvas.create_text(
                    legend_x + 80,
                    legend_y + 25,
                    text="🛣️ Distância | Tempo",
                    font=("Arial", 8, "bold"),
                    fill="darkgreen",
                )
                self.canvas.create_text(
                    legend_x + 80,
                    legend_y + 45,
                    text="Grafo otimizado para distância",
                    font=("Arial", 7),
                    fill="darkgreen",
                )
                self.canvas.create_text(
                    legend_x + 80,
                    legend_y + 62,
                    text="(mostra ambos, destaca distância)",
                    font=("Arial", 6, "italic"),
                    fill="gray",
                )

        # === LEGENDA DE TRÂNSITO (SE ATIVO) ===
        if self.show_traffic_conditions.get() and self.traffic_manager:
            # Posição da legenda (canto inferior direito)
            traffic_legend_x = self.canvas_width - 200
            traffic_legend_y = self.canvas_height - 110

            # Fundo da legenda
            self.canvas.create_rectangle(
                traffic_legend_x - 10,
                traffic_legend_y - 10,
                traffic_legend_x + 190,
                traffic_legend_y + 100,
                fill="white",
                outline="black",
                width=2,
            )

            # Título
            self.canvas.create_text(
                traffic_legend_x + 90,
                traffic_legend_y + 5,
                text="🚦 Condições de Trânsito",
                font=("Arial", 9, "bold"),
                fill="black",
            )

            # Condições
            conditions = [
                ("🟢 Livre", "green", "×1.0"),
                ("🟡 Moderado", "yellow", "×1.3-1.5"),
                ("🟠 Pesado", "orange", "×1.8-2.2"),
                ("🔴 Engarrafado", "red", "×2.5-3.5"),
            ]

            y_offset = 20
            for emoji_label, color, multiplier in conditions:
                # Linha colorida
                self.canvas.create_line(
                    traffic_legend_x + 10,
                    traffic_legend_y + y_offset,
                    traffic_legend_x + 40,
                    traffic_legend_y + y_offset,
                    fill=color,
                    width=3,
                )
                # Texto
                self.canvas.create_text(
                    traffic_legend_x + 50,
                    traffic_legend_y + y_offset,
                    text=f"{emoji_label} {multiplier}",
                    font=("Arial", 8),
                    fill="black",
                    anchor="w",
                )
                y_offset += 18

    def scale_position_vehicle(self, vehicle):
        """Escala posição do veículo (não é um nó, é uma posição real)"""
        # Encontrar bounds dos nós
        if not self.state.graph.nodes:
            return 50, 50

        all_nodes = list(self.state.graph.nodes.values())
        min_lon = min(n.position.longitude for n in all_nodes)
        max_lon = max(n.position.longitude for n in all_nodes)
        min_lat = min(n.position.latitude for n in all_nodes)
        max_lat = max(n.position.latitude for n in all_nodes)

        margin = 50
        available_width = self.canvas_width - 2 * margin
        available_height = self.canvas_height - 2 * margin

        lon_range = max_lon - min_lon or 0.0001
        lat_range = max_lat - min_lat or 0.0001

        x = (
            margin
            + (vehicle.position.longitude - min_lon) / lon_range * available_width
        )
        y = (
            margin
            + (vehicle.position.latitude - min_lat) / lat_range * available_height
        )

        return x, y

    def scale_position_station(self, station):
        """Escala posição de uma estação"""
        # Usar mesma lógica que veículos
        if not self.state.graph.nodes:
            return 50, 50

        all_nodes = list(self.state.graph.nodes.values())
        min_lon = min(n.position.longitude for n in all_nodes)
        max_lon = max(n.position.longitude for n in all_nodes)
        min_lat = min(n.position.latitude for n in all_nodes)
        max_lat = max(n.position.latitude for n in all_nodes)

        margin = 50
        available_width = self.canvas_width - 2 * margin
        available_height = self.canvas_height - 2 * margin

        lon_range = max_lon - min_lon or 0.0001
        lat_range = max_lat - min_lat or 0.0001

        x = (
            margin
            + (station.position.longitude - min_lon) / lon_range * available_width
        )
        y = (
            margin
            + (station.position.latitude - min_lat) / lat_range * available_height
        )

        return x, y

    def run(self):
        """Inicia a interface"""
        self.root.mainloop()


def run_viewer(simulation_engine):
    """Inicia o visualizador com o motor de simulação

    Args:
        simulation_engine: Motor de simulação já inicializado
    """
    root = tk.Tk()
    viewer = TaxiGreenViewerAnimated(root, simulation_engine)
    viewer.run()
