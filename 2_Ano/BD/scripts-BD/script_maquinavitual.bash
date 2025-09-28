#!/bin/bash

# Cria ambiente virtual se não existir
if [ ! -d ".venv" ]; then
    python -m venv .venv
fi

# Ativa ambiente
source .venv/bin/activate

# Instala dependências
pip install psycopg2-binary mysql-connector-python pandas

echo "Ambiente configurado! Para usar:"
echo "source .venv/bin/activate"
echo "python3 test_env.py"