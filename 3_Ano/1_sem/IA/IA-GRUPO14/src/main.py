#!/usr/bin/env python3
"""
TaxiGreen - Sistema de Otimização de Frota de Táxis Heterogénea
Projeto de Inteligência Artificial - Universidade do Minho

Este sistema desenvolve algoritmos de procura para otimizar a gestão de uma frota
de táxis mista (combustão e elétricos), garantindo eficiência operacional e
sustentabilidade ambiental.
"""

import sys
import os

# Adicionar diretório src ao path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
sys.path.insert(0, os.path.dirname(__file__))

from load_dataset import load_dataset
from simulation import SimulationEngine
from ui import run_viewer


def main():
    """
    Função principal que carrega o dataset, inicializa a simulação e inicia a UI
    """

    try:
        print("=" * 60)
        print("  TaxiGreen - Sistema de Otimização de Frota de Táxis")
        print("=" * 60)
        print()

        # Caminho do dataset
        dataset_path = os.path.join(
            os.path.dirname(__file__), "..", "data", "braga_small.json"
        )

        print(f"Carregando dataset: {dataset_path}")

        # Carregar dataset
        state = load_dataset(dataset_path)

        print("Dataset carregado com sucesso!")
        print(f"  - Nós do grafo: {len(state.graph.nodes)}")
        print(f"  - Arestas: {len(state.graph.edges)}")
        print(f"  - Veículos: {len(state.vehicles)}")
        print(f"  - Estações: {len(state.stations)}")
        print()

        # Inicializar motor de simulação
        print("Inicializando motor de simulação...")
        engine = SimulationEngine(state)
        engine.algorithm = "a_star"  # Algoritmo padrão
        print("✓ Motor de simulação pronto!")
        print()

        # Listar veículos
        print("Frota de Táxis:")
        print("-" * 60)
        for vehicle in state.vehicles:
            print(
                f"  {vehicle.name:10} | Tipo: {vehicle.type.value:12} | "
                f"Autonomia: {vehicle.current_autonomy:6.1f}/{vehicle.max_autonomy:6.1f} km | "
                f"Passageiros: {vehicle.passenger_capacity}"
            )
        print()

        # Listar estações
        print("Estações de Recarga/Abastecimento:")
        print("-" * 60)
        for station in state.stations:
            print(
                f"  {station.name:30} | Tipo: {station.type:20} | "
                f"Slots: {station.available_slots}/{station.max_slots}"
            )
        print()

        print("Iniciando interface gráfica...")
        print()

        # Iniciar viewer
        run_viewer(engine)

    except FileNotFoundError as e:
        print(f"Erro: Arquivo não encontrado - {e}")
        print()
        print(
            "Por favor, certifique-se de que o dataset existe em: data/braga_data.json"
        )
        sys.exit(1)
    except Exception as e:
        print(f"❌ Erro: {e}")
        import traceback

        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
