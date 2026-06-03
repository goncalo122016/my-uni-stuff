"""
Motor de simulação automática para gestão de frota de táxis
Gera pedidos aleatórios e atribui veículos com base em distância e preferências
"""

import time
import random
from typing import List, Optional, Dict, Tuple
from models.customer import Customer, generate_random_customer
from models.vehicle import Vehicle


class AutoSimulationEngine:
    """
    Motor de simulação automática que:
    - Gera pedidos de clientes aleatoriamente
    - Atribui veículos com base em proximidade e preferências
    - Rastreia métricas de desempenho da frota
    """

    def __init__(self, vehicles: List[Vehicle], graph, stations, algorithm_finder):
        """
        Args:
            vehicles: Lista de veículos disponíveis
            graph: Grafo da cidade
            stations: Lista de estações de abastecimento/recarga
            algorithm_finder: Função para encontrar caminhos (callback)
        """
        self.vehicles = vehicles
        self.graph = graph
        self.stations = stations
        self.algorithm_finder = algorithm_finder

        # Estado da simulação
        self.is_running = False
        self.start_time = None
        self.customers: List[Customer] = []
        self.active_vehicles: Dict[
            str, dict
        ] = {}  # {vehicle_id: {customer, path, current_step, status}}

        # Configurações
        self.request_interval = (10, 30)  # Gerar pedido a cada 10-30 segundos
        self.last_request_time = 0

        # Métricas
        self.total_requests = 0
        self.completed_trips = 0
        self.rejected_requests = 0
        self.total_wait_time = 0.0
        self.total_trip_time = 0.0

    def _find_nearest_node_to_position(self, position) -> Optional[str]:
        """
        Encontra o nó do grafo mais próximo de uma posição geográfica

        Args:
            position: Objeto Position com latitude/longitude

        Returns:
            node_id do nó mais próximo, ou None se não encontrar
        """
        nearest_node_id = None
        min_distance = float("inf")

        for node_id, node in self.graph.nodes.items():
            # Calcular distância euclidiana simples (lat/lon)
            lat_diff = node.position.latitude - position.latitude
            lon_diff = node.position.longitude - position.longitude
            distance = (lat_diff**2 + lon_diff**2) ** 0.5

            if distance < min_distance:
                min_distance = distance
                nearest_node_id = node_id

        return nearest_node_id

    def start_simulation(self):
        """Inicia a simulação automática"""
        self.is_running = True
        self.start_time = time.time()
        self.last_request_time = self.start_time
        print(" Simulação automática iniciada!")

    def stop_simulation(self):
        """Para a simulação automática"""
        self.is_running = False
        print(" Simulação automática parada!")

    def reset_simulation(self):
        """Reseta a simulação automática ao estado inicial"""
        print(" Resetando simulação automática...")

        # Parar simulação se estiver rodando
        self.is_running = False

        # Limpar todos os dados
        self.customers.clear()
        self.active_vehicles.clear()

        # Resetar métricas
        self.total_requests = 0
        self.completed_trips = 0
        self.rejected_requests = 0
        self.total_wait_time = 0.0
        self.total_trip_time = 0.0

        # Resetar timestamps
        self.start_time = None
        self.last_request_time = 0

        # Resetar autonomia de todos os veículos para 100%
        for vehicle in self.vehicles:
            vehicle.current_autonomy = vehicle.max_autonomy

        print(" Simulação resetada! Todos os dados limpos.")

    def update(self, current_time: float):
        """
        Atualiza o estado da simulação

        Args:
            current_time: Timestamp atual
        """
        if not self.is_running:
            return

        # Gerar novos pedidos aleatoriamente
        if current_time - self.last_request_time >= random.uniform(
            *self.request_interval
        ):
            self._generate_customer_request(current_time)
            self.last_request_time = current_time

        # Tentar atribuir veículos a clientes aguardando (sem veículo atribuído)
        waiting_customers = [
            c for c in self.customers if not c.assigned_vehicle_id and not c.pickup_time
        ]
        if waiting_customers:
            print(
                f" {len(waiting_customers)} clientes aguardando atribuição...",
                flush=True,
            )
        for customer in waiting_customers:
            assigned = self._assign_vehicle_to_customer(customer, current_time)
            if not assigned:
                # Cliente continua aguardando
                wait_time = customer.get_wait_time(current_time)
                if wait_time > 2.0:  # Mais de 2 minutos esperando
                    print(
                        f" {customer.id} aguardando há {wait_time:.1f}min!", flush=True
                    )

        # Atualizar estado dos veículos ativos
        self._update_active_vehicles(current_time)

    def _generate_customer_request(self, current_time: float):
        """Gera um novo pedido de cliente"""
        customer_id = f"C-{self.total_requests + 1:03d}"

        try:
            # Obter nós disponíveis
            available_nodes = list(self.graph.nodes.keys())

            # Gerar cliente aleatório
            customer = generate_random_customer(
                customer_id, available_nodes, current_time
            )
            self.customers.append(customer)
            self.total_requests += 1

            print(
                f" Novo pedido: {customer_id} de {customer.pickup_location} → {customer.dropoff_location}",
                flush=True,
            )
            print(
                f"   Preferência: {customer.preferences.environmental_preference}, Max espera: {customer.preferences.max_wait_time_minutes:.1f}min",
                flush=True,
            )

            # Tentar atribuir veículo
            assigned = self._assign_vehicle_to_customer(customer, current_time)

            # Se não conseguiu atribuir agora (todos ocupados), tentará na próxima iteração
            # NUNCA rejeita - apenas aguarda!
            if not assigned:
                print(
                    f" {customer_id} aguardando veículo ficar disponível...", flush=True
                )

        except Exception as e:
            print(f"Erro ao gerar pedido: {e}", flush=True)

    def _assign_vehicle_to_customer(
        self, customer: Customer, current_time: float
    ) -> bool:
        """
        Atribui o melhor veículo disponível ao cliente
        NUNCA rejeita pedidos - sempre encontra um veículo!

        Args:
            customer: Cliente que precisa de veículo
            current_time: Timestamp atual

        Returns:
            True (sempre consegue atribuir)
        """
        # Encontrar veículos disponíveis (sem filtro de combustível)
        available_vehicles = [
            v for v in self.vehicles if v.id not in self.active_vehicles
        ]

        print(
            f" Veículos totais: {len(self.vehicles)}, Disponíveis: {len(available_vehicles)}, Ativos: {len(self.active_vehicles)}",
            flush=True,
        )

        # Se não há disponíveis, aguardar - mas NUNCA rejeitar!
        if not available_vehicles:
            print(
                f" Todos os veículos ocupados - cliente {customer.id} aguarda",
                flush=True,
            )
            # Cliente fica na fila aguardando, não é rejeitado
            return False  # Retorna False mas não incrementa rejected_requests

        # Filtrar por preferência ambiental se possível
        preferred_vehicles = available_vehicles
        if customer.preferences.prefers_electric():
            electric_vehicles = [
                v for v in available_vehicles if v.fuel_type.value == "electric"
            ]
            print(
                f" Cliente prefere elétrico: {len(electric_vehicles)} disponíveis",
                flush=True,
            )
            if electric_vehicles:
                preferred_vehicles = electric_vehicles
            else:
                print(" Sem elétricos disponíveis, usando qualquer veículo", flush=True)

        # Encontrar veículo mais próximo
        best_vehicle = None
        min_distance = float("inf")
        best_path = None

        for vehicle in preferred_vehicles:
            # Obter posição atual do veículo
            vehicle_node = (
                vehicle.position.node_id
                if hasattr(vehicle.position, "node_id")
                else None
            )

            if not vehicle_node:
                # Encontrar nó mais próximo
                min_dist = float("inf")
                for node_id, node in self.graph.nodes.items():
                    dist = (
                        (vehicle.position.longitude - node.position.longitude) ** 2
                        + (vehicle.position.latitude - node.position.latitude) ** 2
                    ) ** 0.5
                    if dist < min_dist:
                        min_dist = dist
                        vehicle_node = node_id
                print(
                    f" {vehicle.id} sem node_id, nó mais próximo: {vehicle_node}",
                    flush=True,
                )
            else:
                print(f" {vehicle.id} no nó: {vehicle_node}", flush=True)

            # Encontrar caminho até o cliente
            try:
                print(
                    f" Buscando caminho de {vehicle_node} → {customer.pickup_location}",
                    flush=True,
                )
                path = self.algorithm_finder(vehicle_node, customer.pickup_location)
                print(
                    f" Caminho encontrado: {len(path) if path else 0} nós", flush=True
                )
                if path and len(path) > 0:
                    distance = len(path)  # Simplificado: número de nós

                    if distance < min_distance:
                        min_distance = distance
                        best_vehicle = vehicle
                        best_path = path
            except Exception as e:
                print(f" Erro ao buscar caminho para {vehicle.id}: {e}", flush=True)
                import traceback

                traceback.print_exc()
                continue

        print(
            f"Melhor veículo: {best_vehicle.id if best_vehicle else 'NENHUM'}, Caminho: {len(best_path) if best_path else 0} nós",
            flush=True,
        )

        # Se não encontrou caminho, usar primeiro veículo disponível mesmo assim!
        if not best_vehicle:
            best_vehicle = preferred_vehicles[0]
            # Usar caminho direto: veículo já está no nó do cliente ou usar nó mais próximo
            vehicle_node = (
                best_vehicle.position.node_id
                if hasattr(best_vehicle.position, "node_id")
                else list(self.graph.nodes.keys())[0]
            )
            best_path = (
                [vehicle_node, customer.pickup_location]
                if vehicle_node != customer.pickup_location
                else [vehicle_node]
            )
            print(
                f" Sem caminho encontrado, usando {best_vehicle.id} com caminho direto",
                flush=True,
            )
            return False

        # Calcular caminho completo: veículo → pickup → dropoff
        try:
            # SEGURANÇA: Se pickup == dropoff, o cliente já está no destino!
            if customer.pickup_location == customer.dropoff_location:
                print(
                    f" Cliente {customer.id} com pickup == dropoff ({customer.pickup_location}), completando imediatamente",
                    flush=True,
                )
                customer.pickup_time = current_time
                customer.dropoff_time = current_time
                return True

            dropoff_path = self.algorithm_finder(
                customer.pickup_location, customer.dropoff_location
            )
            if not dropoff_path or len(dropoff_path) == 0:
                # Fallback: caminho direto
                dropoff_path = [customer.pickup_location, customer.dropoff_location]
        except:
            dropoff_path = [customer.pickup_location, customer.dropoff_location]

        full_path = best_path + dropoff_path[1:]  # Evitar duplicar o nó de pickup

        # MARCAR VEÍCULO COMO ATRIBUÍDO AO CLIENTE
        customer.assigned_vehicle_id = best_vehicle.id

        self.active_vehicles[best_vehicle.id] = {
            "customer": customer,
            "path": full_path,
            "pickup_node": customer.pickup_location,
            "current_step": 0,
            "status": "going_to_pickup",
            "last_update": current_time,
        }

        print(f" {best_vehicle.id} atribuído a {customer.id}", flush=True)
        print(
            f" Caminho: {len(full_path)} nós - Veículo em {best_path[0]} → Pickup em {customer.pickup_location} → Dropoff em {customer.dropoff_location}",
            flush=True,
        )
        print(f" Clientes aguardando: {len(self.get_waiting_customers())}", flush=True)

        return True  # SEMPRE retorna True - pedido aceito!

    def _send_vehicle_to_refuel(self, vehicle, vehicle_data, current_time: float):
        """
        Envia veículo para estação de abastecimento mais próxima

        Args:
            vehicle: Veículo que precisa reabastecer
            vehicle_data: Dados do veículo ativo
            current_time: Timestamp atual
        """
        # Obter posição atual do veículo
        vehicle_node = (
            vehicle.position.node_id if hasattr(vehicle.position, "node_id") else None
        )

        if not vehicle_node or vehicle_node not in self.graph.nodes:
            print(
                f"  {vehicle.id} sem posição válida ({vehicle_node}), não pode reabastecer",
                flush=True,
            )
            return

        print(
            f" Procurando estação para {vehicle.id} (tipo: {vehicle.fuel_type.value}, posição: {vehicle_node})",
            flush=True,
        )

        # Encontrar estação compatível mais próxima
        best_station = None
        best_distance = float("inf")
        best_path = None
        stations_checked = 0
        stations_available = 0

        for station in self.stations:
            stations_checked += 1

            # Verificar compatibilidade: elétrico → charging_station, combustível → refuel_station
            if (
                vehicle.fuel_type.value == "electric"
                and station.type != "charging_station"
            ):
                continue
            if (
                vehicle.fuel_type.value != "electric"
                and station.type != "refuel_station"
            ):
                continue

            # Verificar se estação está disponível
            if not station.is_available():
                continue

            stations_available += 1
            stations_available += 1

            # Encontrar nó mais próximo da estação (estações têm position, não node_id)
            station_node = self._find_nearest_node_to_position(station.position)
            if not station_node:
                print(
                    f"  Não foi possível encontrar nó próximo a {station.name}",
                    flush=True,
                )
                continue

            # Encontrar caminho até a estação
            try:
                path = self.algorithm_finder(vehicle_node, station_node)
                if path and len(path) > 0:
                    distance = len(path)
                    if distance < best_distance:
                        best_distance = distance
                        best_station = station
                        best_path = path
            except Exception as e:
                print(
                    f"  Erro ao calcular caminho para {station.name}: {e}", flush=True
                )
                continue

        print(
            f" Estações verificadas: {stations_checked}, disponíveis: {stations_available}, melhor: {best_station.name if best_station else 'Nenhuma'}",
            flush=True,
        )

        if not best_station or not best_path:
            print(f" Nenhuma estação disponível para {vehicle.id}", flush=True)
            return

        # Guardar cliente original se houver
        original_customer = vehicle_data.get("customer")
        original_path_after_refuel = vehicle_data.get("path")
        original_step = vehicle_data.get("current_step", 0)
        original_status = vehicle_data.get("status")

        # Atualizar para modo reabastecimento
        vehicle_data["status"] = "refueling"
        vehicle_data["refuel_station"] = best_station
        vehicle_data["path"] = best_path
        vehicle_data["current_step"] = 0
        vehicle_data["last_update"] = current_time
        vehicle_data["original_customer"] = original_customer
        vehicle_data["original_path"] = original_path_after_refuel
        vehicle_data["original_step"] = original_step
        vehicle_data["original_status"] = original_status

        print(
            f" {vehicle.id} indo para {best_station.name} ({best_distance} nós)",
            flush=True,
        )

    def _update_active_vehicles(self, current_time: float):
        """Atualiza posição e status dos veículos ativos"""
        completed_vehicles = []

        for vehicle_id, data in self.active_vehicles.items():
            # Encontrar o veículo
            vehicle = None
            for v in self.vehicles:
                if v.id == vehicle_id:
                    vehicle = v
                    break

            if not vehicle:
                continue

            # VERIFICAR COMBUSTÍVEL - Se <= 10% E NÃO está indo para reabastecer, iniciar reabastecimento
            fuel_percentage = (vehicle.current_autonomy / vehicle.max_autonomy) * 100
            if fuel_percentage <= 10.0 and data.get("status") != "refueling":
                print(
                    f" {vehicle_id} com {fuel_percentage:.1f}% autonomia - Tentando reabastecer!",
                    flush=True,
                )
                self._send_vehicle_to_refuel(vehicle, data, current_time)

                # Verificar se conseguiu definir rota de reabastecimento
                if data.get("status") == "refueling":
                    print(
                        f" {vehicle_id} agora em modo reabastecimento - indo para estação",
                        flush=True,
                    )
                    # NÃO faz continue! Deixa continuar para mover o veículo
                else:
                    print(
                        f" {vehicle_id} não conseguiu iniciar reabastecimento, continuando missão com {fuel_percentage:.1f}%",
                        flush=True,
                    )

            # Simular progresso do veículo (avança no caminho) - INDEPENDENTE DO STATUS
            elapsed = current_time - data["last_update"]

            if elapsed >= 0.8:  # Avançar a cada 0.8 segundos (mais rápido)
                path = data["path"]
                current_step = data["current_step"]

                # Avançar para o próximo nó
                if current_step < len(path) - 1:
                    data["current_step"] += 1
                    current_step = data["current_step"]

                    # Atualizar posição do veículo no grafo
                    current_node_id = path[current_step]
                    if current_node_id in self.graph.nodes:
                        node = self.graph.nodes[current_node_id]
                        vehicle.position.longitude = node.position.longitude
                        vehicle.position.latitude = node.position.latitude
                        vehicle.position.node_id = current_node_id

                        # CONSUMIR AUTONOMIA ao mover (aproximadamente 5 km por nó)
                        distance_km = 5.0  # Distância estimada por aresta
                        vehicle.current_autonomy = max(
                            0, vehicle.current_autonomy - distance_km
                        )

                        print(
                            f" {vehicle_id} moveu para {current_node_id} (passo {current_step}/{len(path) - 1}) - Autonomia: {(vehicle.current_autonomy / vehicle.max_autonomy) * 100:.1f}%",
                            flush=True,
                        )

                data["last_update"] = current_time

                # === MODO REABASTECIMENTO ===
                if data.get("status") == "refueling":
                    # Verificar se chegou à estação
                    if current_step >= len(path) - 1:
                        station = data.get("refuel_station")
                        if station:
                            # Reabastecer completamente
                            vehicle.current_autonomy = vehicle.max_autonomy
                            print(
                                f" {vehicle_id} REABASTECIDO em {station.name} - 100% autonomia!",
                                flush=True,
                            )

                            # Restaurar missão original
                            original_customer = data.get("original_customer")
                            if original_customer and not original_customer.dropoff_time:
                                # Voltar a transportar cliente
                                print(
                                    f"{vehicle_id} retomando transporte de {original_customer.id}",
                                    flush=True,
                                )

                                # Calcular novo caminho do posto até o destino
                                try:
                                    current_node = vehicle.position.node_id
                                    if data.get("original_status") == "going_to_pickup":
                                        new_path = self.algorithm_finder(
                                            current_node,
                                            original_customer.pickup_location,
                                        )
                                        dropoff_path = self.algorithm_finder(
                                            original_customer.pickup_location,
                                            original_customer.dropoff_location,
                                        )
                                        full_path = new_path + dropoff_path[1:]
                                        data["path"] = full_path
                                        data["current_step"] = 0
                                        data["status"] = "going_to_pickup"
                                    else:  # 'transporting'
                                        new_path = self.algorithm_finder(
                                            current_node,
                                            original_customer.dropoff_location,
                                        )
                                        data["path"] = new_path
                                        data["current_step"] = 0
                                        data["status"] = "transporting"
                                except:
                                    # Se falhar, completar viagem
                                    completed_vehicles.append(vehicle_id)
                            else:
                                # Sem cliente, ficar disponível
                                completed_vehicles.append(vehicle_id)
                        continue

                customer = data.get("customer")
                if not customer:
                    continue

                # Verificar se chegou ao pickup (verificar nó atual do veículo)
                if data["status"] == "going_to_pickup":
                    # Pegar posição atual do veículo
                    current_node = (
                        vehicle.position.node_id
                        if hasattr(vehicle.position, "node_id")
                        else None
                    )

                    # Verificar se chegou ao pickup
                    if current_node == customer.pickup_location:
                        # Chegou ao local de pickup
                        data["status"] = "transporting"
                        customer.pickup_time = current_time
                        self.total_wait_time += customer.get_wait_time(current_time)
                        print(
                            f"{vehicle_id} PEGOU {customer.id} em {customer.pickup_location} (espera: {customer.get_wait_time(current_time):.1f}min)",
                            flush=True,
                        )
                    elif current_step >= len(path) - 1:
                        # Chegou ao fim do caminho mas não está no pickup - recalcular
                        print(
                            f" {vehicle_id} chegou ao fim do caminho mas não no pickup, recalculando...",
                            flush=True,
                        )
                        try:
                            new_path = self.algorithm_finder(
                                current_node, customer.pickup_location
                            )
                            if new_path and len(new_path) > 1:
                                data["path"] = (
                                    new_path
                                    + self.algorithm_finder(
                                        customer.pickup_location,
                                        customer.dropoff_location,
                                    )[1:]
                                )
                                data["current_step"] = 0
                            else:
                                # Se já está no local, considerar como pickup
                                data["status"] = "transporting"
                                customer.pickup_time = current_time
                                self.total_wait_time += customer.get_wait_time(
                                    current_time
                                )
                        except:
                            # Fallback: considerar como pickup
                            data["status"] = "transporting"
                            customer.pickup_time = current_time

                # Verificar se completou a viagem
                elif data["status"] == "transporting":
                    if current_step >= len(path) - 1:
                        # Chegou ao destino
                        customer.dropoff_time = current_time
                        self.completed_trips += 1
                        self.total_trip_time += customer.get_total_trip_time() or 0
                        completed_vehicles.append(vehicle_id)
                        print(
                            f" {vehicle_id} COMPLETOU viagem de {customer.id} em {customer.dropoff_location} (tempo: {customer.get_total_trip_time():.1f}min)",
                            flush=True,
                        )

        # Remover veículos que completaram
        for vehicle_id in completed_vehicles:
            del self.active_vehicles[vehicle_id]

    def get_statistics(self) -> dict:
        """Retorna estatísticas da simulação"""
        if not self.start_time:
            return {}

        simulation_time = (time.time() - self.start_time) / 60.0  # em minutos

        # Taxa de ocupação da frota
        occupied_vehicles = len(self.active_vehicles)
        total_vehicles = len(self.vehicles)
        occupancy_rate = (
            (occupied_vehicles / total_vehicles * 100) if total_vehicles > 0 else 0
        )

        # Tempo médio de resposta
        avg_wait_time = (
            (self.total_wait_time / self.completed_trips)
            if self.completed_trips > 0
            else 0
        )

        # Tempo médio de viagem
        avg_trip_time = (
            (self.total_trip_time / self.completed_trips)
            if self.completed_trips > 0
            else 0
        )

        # Taxa de aceitação
        acceptance_rate = (
            ((self.total_requests - self.rejected_requests) / self.total_requests * 100)
            if self.total_requests > 0
            else 0
        )

        return {
            "simulation_time_minutes": simulation_time,
            "total_requests": self.total_requests,
            "completed_trips": self.completed_trips,
            "rejected_requests": self.rejected_requests,
            "active_vehicles": occupied_vehicles,
            "total_vehicles": total_vehicles,
            "occupancy_rate": occupancy_rate,
            "avg_wait_time_minutes": avg_wait_time,
            "avg_trip_time_minutes": avg_trip_time,
            "acceptance_rate": acceptance_rate,
            "customers": self.customers,
        }

    def get_waiting_customers(self) -> List[Customer]:
        """Retorna lista de clientes aguardando pickup (com ou sem veículo atribuído)"""
        return [
            c for c in self.customers if not c.pickup_time
        ]  # Todos que ainda não foram pegos

    def get_customers_in_transit(self) -> List[Customer]:
        """Retorna lista de clientes sendo transportados"""
        return [c for c in self.customers if c.pickup_time and not c.dropoff_time]

    def get_vehicle_path(self, vehicle_id: str) -> Optional[list]:
        """Retorna o caminho atual de um veículo ativo"""
        if vehicle_id in self.active_vehicles:
            return self.active_vehicles[vehicle_id]["path"]
        return None

    def get_vehicle_customer(self, vehicle_id: str) -> Optional[Customer]:
        """Retorna o cliente atribuído a um veículo"""
        if vehicle_id in self.active_vehicles:
            return self.active_vehicles[vehicle_id]["customer"]
        return None
