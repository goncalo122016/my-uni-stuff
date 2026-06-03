#!/bin/bash
# run_normal_test.sh

# Prefer repo-root relative paths instead of relying on $HOME
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

MOUNT_POINT="$REPO_ROOT/codebase/tmp/meu_fuse/tmp"
RESULTS_DIR="$REPO_ROOT/tests/fio_results"
FUSE_BIN="$REPO_ROOT/codebase/passthrough_normal"
BPF_DIR="$REPO_ROOT/tests/BPF"

mkdir -p "$RESULTS_DIR"

# Limpar ficheiro de estatísticas do disco para cada execução completa
> "$RESULTS_DIR/normal_disk_usage.txt"

for DEDUPE in 0 50 100; do
    echo "=== Teste BASELINE (Normal) com carga de ${DEDUPE}% deduplicação ==="

    # Limpar backend na totalidade (ficheiros normais + metadados ocultos)
    sudo find /backend -mindepth 1 -delete 2>/dev/null

    # Montar FUSE em background (com módulo subdir para mapear para /backend)
    $FUSE_BIN -f -o modules=subdir,subdir=/backend $MOUNT_POINT > /dev/null 2>&1 &
    FUSE_PID=$!
    sleep 1
    echo "FUSE PID: $FUSE_PID"

    # Iniciar tracer BPF em background
    sudo $BPF_DIR/systracer $FUSE_PID > $RESULTS_DIR/normal_bpf_${DEDUPE}.txt 2>&1 &
    BPF_PID=$!
    sleep 1

    # Iniciar monitorização de recursos (CPU e Memória) do processo FUSE
    pidstat -p $FUSE_PID -r -u 1 > $RESULTS_DIR/normal_fuse_resources_${DEDUPE}.txt &
    PIDSTAT_PID=$!

    # Correr FIO
    fio --name=normal_dedupe_${DEDUPE} \
        --directory=$MOUNT_POINT \
        --rw=write --bs=4k --size=100m \
        --ioengine=psync \
        --dedupe_percentage=$DEDUPE \
        --direct=1 \
        --output=$RESULTS_DIR/normal_dedupe_${DEDUPE}.json \
        --output-format=json

    # Recolher espaço usado (agora não há .dedupblocks, os ficheiros ficam no /backend em bruto)
    echo "Ficheiros únicos: $(find /backend -type f | wc -l)" \
        >> $RESULTS_DIR/normal_disk_usage.txt
    echo "Espaço usado: $(du -sh /backend)" \
        >> $RESULTS_DIR/normal_disk_usage.txt

    # Parar tracer e monitorização
    sudo kill $BPF_PID 2>/dev/null
    kill $PIDSTAT_PID 2>/dev/null

    fusermount3 -u $MOUNT_POINT 2>/dev/null || sudo umount $MOUNT_POINT
    sleep 1
    sudo kill -9 $FUSE_PID 2>/dev/null # Garantir que o processo FUSE termina mesmo
    wait $FUSE_PID 2>/dev/null

    # Processar os resultados do BPF para extrair a métrica de "dados escritos"
    echo "Resumo de I/O BPF (Layer 0=Syscall, Layer 2=EXT4):" >> $RESULTS_DIR/normal_disk_usage.txt
    awk 'NR>1 {
        layer=$2; syscall_id=$4; syscall_name=$5; bytes=$6;
        if(layer == 0 && syscall_id == 1) write_l0 += bytes;
        if(layer == 2 && syscall_name == "setitimer") write_l2 += bytes;
    } END {
        # Fallback to 0 if variables are empty
        if (!write_l0) write_l0 = 0;
        if (!write_l2) write_l2 = 0;
        print "  VFS Escrito (Bytes): " write_l0;
        print "  Block/EXT4 Escrito (Bytes): " write_l2;
    }' $RESULTS_DIR/normal_bpf_${DEDUPE}.txt >> $RESULTS_DIR/normal_disk_usage.txt

    echo "=== Resultado ${DEDUPE}% concluído ==="
    sleep 2
done

echo "Todos os testes FIO concluídos! Resultados em $RESULTS_DIR"

echo "=== Teste de Carga de Trabalho Criada Manualmente ==="
# Limpar estado
sudo find /backend -mindepth 1 -delete 2>/dev/null
$FUSE_BIN -f -o modules=subdir,subdir=/backend $MOUNT_POINT > /dev/null 2>&1 &
FUSE_PID=$!
sleep 1

echo "A copiar ficheiros (Carga manual)..."
dd if=/dev/urandom of=$MOUNT_POINT/file_base.bin bs=1M count=10 2>/dev/null
cp $MOUNT_POINT/file_base.bin $MOUNT_POINT/file_dup1.bin
cp $MOUNT_POINT/file_base.bin $MOUNT_POINT/file_dup2.bin

echo "Ficheiros únicos manuais: $(find /backend -type f | wc -l)" >> $RESULTS_DIR/normal_disk_usage.txt
echo "Espaço usado manual: $(du -sh /backend)" >> $RESULTS_DIR/normal_disk_usage.txt

fusermount3 -u $MOUNT_POINT 2>/dev/null || sudo umount $MOUNT_POINT
sudo kill -9 $FUSE_PID 2>/dev/null

echo "Fim."
