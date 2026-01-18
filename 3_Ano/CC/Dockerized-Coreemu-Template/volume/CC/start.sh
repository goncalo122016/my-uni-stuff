#!/bin/bash

echo "=== Iniciando Sistema ==="

# Verificar se CORE está rodando
if ! docker ps | grep -q "core"; then
    echo "Erro: Container CORE não está rodando!"
    exit 1
fi

# Função para iniciar componente em background
start_component() {
    local name=$1
    local command=$2
    
    echo "Iniciando $name..."
    gnome-terminal --title="$name" -- bash -c "$command; read -p 'Pressione Enter para fechar...'"
    sleep 2
}

# Variável ROOT do projeto dentro do container
PROJECT_PATH="/volume/CC"

# Comando base para injetar PYTHONPATH e arrancar o Python corretamente
PY_ENV="export PYTHONPATH=$PROJECT_PATH && cd $PROJECT_PATH"

# ===== MotherShip =====
start_component "MotherShip" \
"docker exec -it core vcmd -c /tmp/pycore.1/MotherShip -- \
bash -c '$PY_ENV && python3 -m mothership.mothership_main --host 10.0.0.20 --api-host 10.0.8.20'"

echo "Aguardando MotherShip inicializar..."
#sleep 5

# ===== Rover 1 =====
start_component "Rover1" \
"docker exec -it core vcmd -c /tmp/pycore.1/rover1 -- \
bash -c '$PY_ENV && python3 -m rover.rover_main --rover-id R-001 --host 10.0.4.20 --mothership 10.0.0.20'"

# ===== Rover 2 (opcional) =====
start_component "Rover2" \
"docker exec -it core vcmd -c /tmp/pycore.1/rover2 -- \
bash -c '$PY_ENV && python3 -m rover.rover_main --rover-id R-002 --host 10.0.5.20 --mothership 10.0.0.20'"

# ===== Rover 3 (opcional) =====
start_component "Rover3" \
"docker exec -it core vcmd -c /tmp/pycore.1/rover3 -- \
bash -c '$PY_ENV && python3 -m rover.rover_main --rover-id R-003 --host 10.0.6.20 --mothership 10.0.0.20'"

# ===== Ground Control =====
start_component "GroundControl" \
"docker exec -it core vcmd -c /tmp/pycore.1/GroundControl -- \
bash -c '$PY_ENV && python3 -m ground_control.ground_control --host 10.0.7.20 --mothership 10.0.8.20'"

# ===== UI SERVER + FIREFOX =====
echo "Iniciando servidor UI no GroundControl..."

docker exec core vcmd -c /tmp/pycore.1/GroundControl -- \
    bash -c "cd /volume/CC/ui && nohup python3 -m http.server 8000 >/dev/null 2>&1 &"

echo "Abrindo Firefox com a interface..."
docker exec core vcmd -c /tmp/pycore.1/GroundControl -- \
    bash -c "firefox http://10.0.7.20:8000 >/dev/null 2>&1 &"

echo "=== Todos os componentes iniciados! ==="
