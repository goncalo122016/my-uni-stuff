#!/bin/bash

# Script para executar os testes do Sistema Distribuído

echo "╔════════════════════════════════════════════════════╗"
echo "║    TESTES - SISTEMA DISTRIBUÍDO                    ║"
echo "╚════════════════════════════════════════════════════╝"
echo ""

if [ ! -f "./gradlew" ]; then
    echo "Erro: gradlew não encontrado"
    exit 1
fi

echo "Compilando projeto..."
./gradlew build -q 2>&1 | grep -i "failed\|error" || echo "✓ Compilação bem-sucedida"
echo ""

echo "Parando processos anteriores..."
pkill -9 -f "java.*Server" 2>/dev/null
sleep 1
echo ""

echo "Iniciando servidor..."
cd app
SERVER_LOG="/tmp/server_$$.log"
java -cp build/classes/java/main server.Server 30 10 > "$SERVER_LOG" 2>&1 &
SERVER_PID=$!
sleep 2

if ! lsof -Pi :12345 -sTCP:LISTEN -t >/dev/null 2>&1; then
    echo "✗ Servidor não conseguiu iniciar"
    echo "Log do servidor:"
    cat "$SERVER_LOG"
    exit 1
fi

echo "✓ Servidor iniciado (PID: $SERVER_PID)"
echo "  (Log do servidor: $SERVER_LOG)"
echo ""

GRADLE_CACHE="$HOME/.gradle/caches/modules-2/files-2.1"

JUNIT_API=$(find "$GRADLE_CACHE" -name "junit-jupiter-api-*.jar" ! -name "*sources*" ! -name "*javadoc*" 2>/dev/null | sort -V | tail -1)
JUNIT_ENGINE=$(find "$GRADLE_CACHE" -name "junit-jupiter-engine-*.jar" ! -name "*sources*" ! -name "*javadoc*" 2>/dev/null | sort -V | tail -1)
JUNIT_COMMONS=$(find "$GRADLE_CACHE" -name "junit-platform-commons-*.jar" ! -name "*sources*" ! -name "*javadoc*" 2>/dev/null | sort -V | tail -1)
OPENTEST=$(find "$GRADLE_CACHE" -name "opentest4j-*.jar" ! -name "*sources*" ! -name "*javadoc*" 2>/dev/null | sort -V | tail -1)
APIGUARDIAN=$(find "$GRADLE_CACHE" -name "apiguardian-api-*.jar" ! -name "*sources*" ! -name "*javadoc*" 2>/dev/null | sort -V | tail -1)

CLASSPATH="build/classes/java/main:build/classes/java/test"
[ -n "$JUNIT_API" ] && CLASSPATH="$CLASSPATH:$JUNIT_API"
[ -n "$JUNIT_ENGINE" ] && CLASSPATH="$CLASSPATH:$JUNIT_ENGINE"
[ -n "$JUNIT_COMMONS" ] && CLASSPATH="$CLASSPATH:$JUNIT_COMMONS"
[ -n "$OPENTEST" ] && CLASSPATH="$CLASSPATH:$OPENTEST"
[ -n "$APIGUARDIAN" ] && CLASSPATH="$CLASSPATH:$APIGUARDIAN"

java -cp "$CLASSPATH" TestRunner 2>/dev/null

echo ""
echo "Encerrando servidor..."
kill $SERVER_PID 2>/dev/null
pkill -9 -f "java.*Server" 2>/dev/null
echo "Servidor encerrado"
echo ""
echo "Para ver o log do servidor: cat $SERVER_LOG"
echo "✓ Teste completo"
