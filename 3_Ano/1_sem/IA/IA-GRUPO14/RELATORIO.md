# TAXIGREEN - RELATÓRIO DO PROJETO

## Sistema de Otimização de Frota de Táxis Heterogénea

---

## 1. DESCRIÇÃO DO PROBLEMA

### 1.1 Contexto

A TaxiGreen é uma empresa de táxis urbanos que enfrenta desafios crescentes na gestão de sua frota. Com a transição progressiva para mobilidade sustentável, a empresa necessita gerir simultaneamente táxis a combustão tradicional e veículos elétricos, cada um com características distintas.

### 1.2 Desafio Principal

Otimizar a gestão da frota mista garantindo:

- Atendimento de todos os pedidos dentro de prazos aceitáveis
- Respetivas limitações específicas de cada tipo de veículo
- Eficiência operacional e redução de custos
- Cumprimento de critérios ambientais

### 1.3 Restrições e Limitações

- **Veículos Elétricos**: Autonomia limitada (300-350 km), necessitam recarga em estações específicas
- **Veículos a Combustão**: Custo operacional superior, maior impacto ambiental
- **Pedidos Dinâmicos**: Chegam ao longo do dia, com diferentes prioridades
- **Capacidade de Passageiros**: Diferenciada entre veículos
- **Trânsito Variável**: Condições que afetam deslocação
- **Disponibilidade de Estações**: Limitadas, podem falhar

---

## 2. FORMULAÇÃO DO PROBLEMA COMO PROCURA

### 2.1 Estado Inicial

- Frota de veículos em posições iniciais e com autonomia máxima
- Conjunto vazio de pedidos ativos
- Todas as estações de recarga operacionais
- Condições de trânsito normais

### 2.2 Teste Objetivo

- Satisfazer todos os pedidos de transporte
- Minimizar tempo total de operação
- Minimizar custos operacionais
- Minimizar emissões de CO2
- Maximizar ocupação da frota

### 2.3 Operadores (Ações Disponíveis)

1. **Alocar Veículo a Pedido**: Assign a vehicle to a customer request
2. **Mover Veículo**: Move vehicle from current position to origin
3. **Executar Transporte**: Drive from origin to destination with passengers
4. **Recarregar Veículo**: Charge electric vehicle at charging station
5. **Reabastecer Veículo**: Refuel combustion vehicle at gas station
6. **Aceitar/Rejeitar Pedido**: Based on availability and constraints

### 2.4 Custo da Solução

A função de custo integra múltiplos critérios:

$$f(solução) = α × T_{resposta} + β × C_{operacional} + γ × E_{CO_2} + δ × P_{rejeição}$$

Onde:

- $T_{resposta}$ = Tempo médio de resposta (minutos)
- $C_{operacional}$ = Custo total em euros
- $E_{CO_2}$ = Emissões de CO2 em kg
- $P_{rejeição}$ = Taxa de pedidos rejeitados

---

## 3. REPRESENTAÇÃO DA CIDADE

### 3.1 Estrutura do Grafo

- **Nós**: Representam localizações na cidade

  - Zonas de recolha de passageiros
  - Estações de recarga (para elétricos)
  - Postos de abastecimento (para combustão)
  - Pontos de interesse (Aeroporto, Estação, Hospital, etc.)

- **Arestas**: Representam caminhos possíveis
  - Peso: Distância (km) e tempo (minutos)
  - Fator de trânsito: Multiplicador que varia ao longo do dia

### 3.2 Dados da Cidade de Braga

**Grafo com 6 nós principais:**

```
0: Centro Histórico (hub central)
1: Zona Industrial (oeste)
2: Bairro Este (residencial)
3: Zona Comercial (comercial)
4: Zona Residencial Oeste
5: Zona Norte
```

**Distâncias aproximadas:**

- Centro a Zona Industrial: 5.2 km (8 min)
- Centro a Zona Comercial: 1.5 km (3 min)
- Centro a Bairro Este: 2.1 km (4 min)

---

## 4. ALGORITMOS DE PROCURA IMPLEMENTADOS

### 4.1 Procura Não Informada

#### BFS (Breadth-First Search)

- **Descrição**: Explora nós por níveis
- **Vantagem**: Encontra caminho com menos nós
- **Desvantagem**: Sem consideração de custos
- **Caso de Uso**: Quando distância em nós é importante

#### DFS (Depth-First Search)

- **Descrição**: Explora em profundidade
- **Vantagem**: Usa pouca memória
- **Desvantagem**: Pode ser ineficiente com grafos grandes
- **Caso de Uso**: Exploração de caminhos alternativos

#### IDS (Iterative Deepening)

- **Descrição**: Combina vantagens de BFS e DFS
- **Vantagem**: Óptimo com limite de profundidade
- **Desvantagem**: Mais iterações
- **Caso de Uso**: Quando limite de profundidade é crítico

#### UCS (Uniform Cost Search)

- **Descrição**: Procura por custo uniforme
- **Vantagem**: Encontra solução com menor custo
- **Desvantagem**: Sem heurística, pode ser lento
- **Caso de Uso**: Minimização de custos operacionais

### 4.2 Procura Informada

#### Greedy Search

- **Descrição**: Escolhe sempre a opção com melhor heurística
- **Heurística**: Distância euclidiana ao destino
- **Vantagem**: Rápido e geralmente bom
- **Desvantagem**: Não garante solução ótima
- **Caso de Uso**: Alocação rápida de veículos

#### A\* Search

- **Descrição**: Combina custo real com heurística
- **Fórmula**: $f(n) = g(n) + h(n)$
- **Vantagem**: Óptimo com heurística admissível
- **Desvantagem**: Mais caro computacionalmente
- **Caso de Uso**: Melhor qualidade de solução

### 4.3 Heurísticas Especializadas

**1. Heurística de Autonomia**

```
penalidade = (1 - autonomia_atual/autonomia_max) × 100
```

Prioriza veículos com mais autonomia.

**2. Heurística de Custo**

```
custo_estimado = distancia_km × custo_operacional_veículo/km
```

Minimiza custos operacionais.

**3. Heurística de Tempo**

```
tempo_estimado = distancia_km / velocidade_média
```

Reduz tempo de deslocação.

**4. Heurística Ambiental**

```
penalidade_ambiental =
  - 20 se exigido elétrico e é elétrico
  - 10 se preferência ambiental e é elétrico
  - 5 se preferência ambiental mas é combustão
```

**5. Heurística Combinada**

```
score = 0.3×tempo + 0.3×custo + 0.2×autonomia + 0.2×ambiental
```

---

## 5. SIMULAÇÃO DINÂMICA

### 5.1 Componentes da Simulação

**SimulationEngine** responsável por:

1. Gerar pedidos aleatoriamente
2. Alocar veículos a pedidos
3. Simular viagens
4. Monitorar autonomia
5. Simular recarga/abastecimento
6. Atualizar métricas

### 5.2 Processo de Alocação

```
1. Novo pedido chega
2. Filtrar veículos disponíveis
3. Para cada veículo:
   a. Calcular score heurístico
   b. Verificar se pode alcançar destino
4. Selecionar veículo com melhor score
5. Se nenhum disponível, rejeitar pedido
```

### 5.3 Dinâmica Temporal

- Passo de simulação: 1 minuto
- Velocidade: Ajustável (0.1x a 3x)
- Duração média de viagem: 5-15 minutos
- Taxa de chegada de pedidos: ~0.3/minuto

---

## 6. MÉTRICAS DE AVALIAÇÃO

### 6.1 Métricas de Serviço

| Métrica              | Descrição                | Unidade | Ideal |
| -------------------- | ------------------------ | ------- | ----- |
| Tempo Médio Resposta | Entre pedido e pickup    | min     | < 5   |
| Taxa Conclusão       | Pedidos atendidos/Total  | %       | > 95  |
| Taxa Rejeição        | Pedidos rejeitados/Total | %       | < 5   |
| Ocupação Média       | Passageiros/Capacidade   | %       | > 70  |

### 6.2 Métricas de Eficiência

| Métrica           | Descrição           | Unidade | Ideal     |
| ----------------- | ------------------- | ------- | --------- |
| Custo Total       | Custos operacionais | €       | Minimizar |
| Custo por km      | Custo/Distância     | €/km    | < 0.25    |
| Distância Vazia   | Km sem passageiros  | km      | Minimizar |
| % Distância Vazia | Vazia/Total         | %       | < 30      |

### 6.3 Métricas Ambientais

| Métrica        | Descrição          | Unidade | Ideal     |
| -------------- | ------------------ | ------- | --------- |
| Emissões CO2   | Total de emissões  | kg      | Minimizar |
| Taxa Elétricos | Veículos elétricos | %       | > 40      |
| CO2 por km     | Emissões/Distância | kg/km   | < 0.15    |

---

## 7. INTERFACE GRÁFICA

### 7.1 Componentes da UI

1. **Painel Superior**: Controles principais

   - Botões: Iniciar, Parar, Resetar, Relatório
   - Exibição de tempo atual

2. **Mapa da Cidade**: Visualização do grafo

   - Nós como pontos azuis
   - Veículos com cores:
     - 🟢 Verde: Ocioso
     - 🔴 Vermelho: Em serviço
   - Tipos distintos: Círculo (combustão) vs Triângulo (elétrico)

3. **Configurações**

   - Seleção de algoritmo
   - Ajuste de velocidade

4. **Estado da Simulação**

   - Pedidos ativos/completados/rejeitados
   - Veículos por estado
   - Métricas em tempo real

5. **Gráficos de Métricas**
   - Distribuição distância (vazia vs carregada)
   - Emissões CO2
   - Taxa de ocupação

---

## 8. EXTENSÕES IMPLEMENTADAS

### 8.1 Funcionalidades Adicionais

1. **Variações de Trânsito**: Multiplicador de tempo/distância
2. **Falhas de Estações**: Simulação de indisponibilidade
3. **Múltiplos Algoritmos**: Comparação em tempo real
4. **Análises Detalhadas**: Relatórios completos
5. **Visualização Responsiva**: Interface adaptável

### 8.2 Possíveis Melhorias

- Predição de procura com padrões históricos
- Ride-sharing para múltiplos passageiros
- Integração com dados meteorológicos
- Sistema de incentivos para elétricos
- Otimização de turnos
- Algoritmos genéticos para otimização

---

## 9. TECNOLOGIAS UTILIZADAS

### Linguagem e Frameworks

- **Python 3.8+**: Linguagem principal
- **Tkinter**: Interface gráfica
- **Matplotlib**: Visualização de gráficos
- **NetworkX**: Manipulação de grafos
- **JSON**: Formato de dados

### Dependências

```
networkx==2.6.3
matplotlib==3.5.3
pillow==10.4.0
osmnx==1.9.4
```

---

## 10. CONCLUSÕES

O projeto TaxiGreen demonstra:

- ✅ Aplicação prática de algoritmos de procura
- ✅ Integração de heurísticas especializadas
- ✅ Simulação dinâmica realista
- ✅ Interface responsiva e intuitiva
- ✅ Análises quantitativas completas

Este sistema pode ser usado para:

- Treinar operadores em tomada de decisão
- Avaliar estratégias de alocação
- Otimizar rotas e recursos
- Promover sustentabilidade ambiental

---

**Projeto desenvolvido em: outubro 2025**  
**Universidade do Minho - Inteligência Artificial**
