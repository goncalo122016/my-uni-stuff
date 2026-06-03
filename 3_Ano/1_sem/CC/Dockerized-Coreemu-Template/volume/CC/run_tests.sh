#!/bin/bash

echo "=== A EXECUTAR TESTES DE MISSÕES ==="

TEST_NUMBER=$1  # opcional

if [[ -n "$TEST_NUMBER" ]]; then
    ARG=" $TEST_NUMBER"
else
    ARG=""
fi

docker exec core vcmd -c /tmp/pycore.1/MotherShip -- \
bash -c "cd /volume/CC && PYTHONPATH=/volume/CC python3 tests/test_runner.py$ARG"

echo "=== TESTES FINALIZADOS ==="
