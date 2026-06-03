"""
Módulo para gerenciamento de condições de trânsito variáveis por hora do dia
"""

import time
import random
from datetime import datetime, timedelta


class TrafficConditionManager:
    """
    Gerenciador de condições de trânsito baseado em hora do dia.
    
    Simula diferentes níveis de congestionamento ao longo do dia:
    - Manhã (9h-10h): tráfego PESADO em algumas ruas (trabalho)
    - Meio do dia (10h-17h): tráfego MODERADO e LIVRE
    - Noite (17h-19h): tráfego PESADO em algumas ruas (saída do trabalho)
    - Madrugada/noite tarde: tráfego LIVRE
    
    Condições disponíveis:
    - LIVRE: 0% de atraso (multiplicador 1.0)
    - MODERADO: 30-50% de atraso (multiplicador 1.3-1.5)
    - PESADO: 80-120% de atraso (multiplicador 1.8-2.2)
    - ENGARRAFADO: 150-250% de atraso (multiplicador 2.5-3.5)
    """
    
    CONDITIONS = {
        'LIVRE': {'min': 1.0, 'max': 1.0, 'color': 'green', 'emoji': '🟢'},
        'MODERADO': {'min': 1.3, 'max': 1.5, 'color': 'yellow', 'emoji': '🟡'},
        'PESADO': {'min': 1.8, 'max': 2.2, 'color': 'orange', 'emoji': '🟠'},
        'ENGARRAFADO': {'min': 2.5, 'max': 3.5, 'color': 'red', 'emoji': '🔴'}
    }
    
    def __init__(self, start_hour=6):
        """
        Args:
            start_hour: Hora inicial da simulação (0-23)
        """
        # Armazenar ruas com tráfego pesado em períodos específicos
        self.rush_hour_streets_morning = set()  # Ruas com tráfego às 9h-10h
        self.rush_hour_streets_evening = set()  # Ruas com tráfego às 17h-19h
        
        self.start_hour = start_hour
        self.start_minute = 0
        self.current_hour = start_hour
        self.current_minute = 0
        self.edge_conditions = {}  # {(node1, node2): {'condition': 'LIVRE', 'multiplier': 1.0, 'hour': hour}}
        
        self.last_update_time = time.time()
        self.update_interval = 1.0  # Atualizar a cada 1 segundo real
        # time_speed = 1.0: 1 segundo real = 1 minuto de simulação
        # Logo: 60 segundos reais = 1 hora simulada
        self.time_speed = 1.0  # Velocidade: 60 segundos reais = 1 hora simulada (mais lento)
        
    def update_simulation_time(self, delta_seconds):
        """
        Avança o tempo da simulação.
        
        Args:
            delta_seconds: Segundos reais decorridos (será multiplicado por time_speed)
        """
        # Calcular quantos minutos de simulação passaram
        minutes_passed = (delta_seconds * self.time_speed)
        old_time = f"{int(self.current_hour):02d}:{int(self.current_minute):02d}"
        self.current_minute += minutes_passed
        
        # Converter minutos extras em horas
        if self.current_minute >= 60:
            extra_hours = int(self.current_minute // 60)
            self.current_hour += extra_hours
            self.current_minute = self.current_minute % 60
            
            # Resetar para 0h se passou 24h
            if self.current_hour >= 24:
                self.current_hour = self.current_hour % 24
        
        new_time = f"{int(self.current_hour):02d}:{int(self.current_minute):02d}"
        log_msg = f"[TrafficManager] delta={delta_seconds:.4f}s | min_passed={minutes_passed:.2f} | {old_time} → {new_time}"
        with open('/tmp/taxigreen_debug.log', 'a') as f:
            f.write(log_msg + '\n')
    
    def get_current_time_str(self):
        """Retorna a hora simulada formatada (HH:MM)"""
        return f"{int(self.current_hour):02d}:{int(self.current_minute):02d}"
    
    def add_travel_time(self, minutes):
        """
        Adiciona tempo de viagem ao relógio.
        
        Args:
            minutes: Número de minutos a adicionar
        """
        old_time = self.get_current_time_str()
        self.current_minute += minutes
        
        # Converter minutos extras em horas
        if self.current_minute >= 60:
            extra_hours = int(self.current_minute // 60)
            self.current_hour += extra_hours
            self.current_minute = self.current_minute % 60
            
            # Resetar para 0h se passou 24h
            if self.current_hour >= 24:
                self.current_hour = self.current_hour % 24
        
        new_time = self.get_current_time_str()
        return old_time, new_time
    
    def get_start_time_str(self):
        """Retorna a hora inicial da simulação"""
        return f"{int(self.start_hour):02d}:{int(self.start_minute):02d}"
    
    def get_simulation_duration_str(self):
        """Retorna a duração da simulação em HH:MM format"""
        # Calcular diferença
        total_minutes = (self.current_hour - self.start_hour) * 60 + (self.current_minute - self.start_minute)
        if total_minutes < 0:
            total_minutes += 24 * 60
        
        hours = int(total_minutes // 60)
        minutes = int(total_minutes % 60)
        return f"{hours:02d}:{minutes:02d}"
    
    def _get_hour_period(self):
        """
        Retorna o período do dia e nível de tráfego esperado.
        
        Returns:
            tuple: (period_name, traffic_level)
                - traffic_level: 'high' (pico), 'medium', 'low' (livre)
        """
        hour = self.current_hour
        
        # Manhã (9h-10h): Tráfego alto (pessoas indo trabalhar)
        if 9 <= hour < 10:
            return ('Manhã - Pico de tráfego', 'high')
        
        # Meio do dia (10h-17h): Tráfego normal
        elif 10 <= hour < 17:
            return ('Meio do dia', 'medium')
        
        # Noite (17h-19h): Tráfego alto (pessoas saindo do trabalho)
        elif 17 <= hour < 19:
            return ('Noite - Pico de tráfego', 'high')
        
        # Resto do dia: Tráfego livre
        else:
            return ('Madrugada/Noite', 'low')
    
    def get_traffic_multiplier(self, node1, node2):
        """
        Retorna o multiplicador de tempo para uma aresta específica baseado na hora.
        
        Args:
            node1, node2: IDs dos nós da aresta
            
        Returns:
            float: Multiplicador de tempo (1.0 = sem atraso, >1.0 = atraso)
        """
        edge_key = tuple(sorted([node1, node2]))
        period_name, traffic_level = self._get_hour_period()
        
        # Determinar se esta aresta deve ter tráfego baseado em hash (determinístico)
        # Isso garante que a mesma rua sempre tem tráfego no mesmo horário
        edge_hash = hash(edge_key) % 100
        
        if traffic_level == 'high':
            # Durante picos: 60% das ruas têm tráfego pesado
            if edge_hash < 60:
                condition_name = 'PESADO'
            else:
                condition_name = 'MODERADO'
        
        elif traffic_level == 'medium':
            # Meio do dia: 30% das ruas têm tráfego moderado
            if edge_hash < 30:
                condition_name = 'MODERADO'
            else:
                condition_name = 'LIVRE'
        
        else:  # 'low'
            # Madrugada: apenas 10% das ruas têm tráfego leve
            if edge_hash < 10:
                condition_name = 'MODERADO'
            else:
                condition_name = 'LIVRE'
        
        # Gerar multiplicador dentro do range da condição
        condition_data = self.CONDITIONS[condition_name]
        multiplier = random.uniform(condition_data['min'], condition_data['max'])
        
        return multiplier
    
    def get_condition_info(self, node1, node2):
        """Retorna informações sobre a condição de tráfego de uma aresta"""
        edge_key = tuple(sorted([node1, node2]))
        period_name, traffic_level = self._get_hour_period()
        
        # Determinar se esta aresta deve ter tráfego
        edge_hash = hash(edge_key) % 100
        
        if traffic_level == 'high':
            if edge_hash < 60:
                condition_name = 'PESADO'
            else:
                condition_name = 'MODERADO'
        elif traffic_level == 'medium':
            if edge_hash < 30:
                condition_name = 'MODERADO'
            else:
                condition_name = 'LIVRE'
        else:
            if edge_hash < 10:
                condition_name = 'MODERADO'
            else:
                condition_name = 'LIVRE'
        
        condition_data = self.CONDITIONS[condition_name]
        return {
            'condition': condition_name,
            'level': condition_name.capitalize(),
            'multiplier': random.uniform(condition_data['min'], condition_data['max']),
            'color': condition_data['color'],
            'emoji': condition_data['emoji'],
            'period': period_name
        }
    
    def reset_all(self):
        """Reseta todas as condições de tráfego e volta ao horário inicial"""
        self.edge_conditions.clear()
        self.current_hour = 6
        self.current_minute = 0
    
    def get_statistics(self):
        """Retorna estatísticas sobre as condições atuais (compatível com código antigo)"""
        # Para compatibilidade com código antigo que usa esta função
        stats = {
            'livre': 0, 
            'moderado': 0, 
            'pesado': 0, 
            'engarrafado': 0, 
            'total': 0,
            'current_time': self.get_current_time_str(),
            'period': self._get_hour_period()[0]
        }
        
        return stats
