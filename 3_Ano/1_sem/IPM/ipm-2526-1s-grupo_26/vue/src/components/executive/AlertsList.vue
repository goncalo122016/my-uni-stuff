<template>
  <div class="rounded-xl border border-gray-300 bg-white p-6 shadow-lg">
    <div class="mb-4 flex items-start justify-between">
      <div>
        <h3 class="text-lg font-bold text-gray-900">Alertas ativos</h3>
        <p class="text-sm text-gray-500">Propriedades que requerem atenção ou ação regulatória</p>

        <!-- Filter buttons -->
        <div class="mt-3 flex items-center gap-3">
          <!-- CRITICAL BUTTON -->
          <button
            @click="activeFilter = 'critical'"
            :class="[
              'flex items-center gap-2 rounded-lg px-3 py-1.5 text-sm transition-colors',
              activeFilter === 'critical'
                ? 'border border-red-400 bg-red-50 text-red-700'
                : 'border border-gray-300 bg-gray-50 text-gray-700 hover:bg-gray-100',
            ]"
          >
            <AlertTriangle class="h-4 w-4 text-red-600" />
            <span class="text-sm font-medium">Crítico</span>
            <span class="rounded border border-gray-300 bg-white px-2 py-0.5 text-xs font-semibold">
              {{ displayCritical }}
            </span>
          </button>

          <!-- WARNING BUTTON -->
          <button
            @click="activeFilter = 'warning'"
            :class="[
              'flex items-center gap-2 rounded-lg px-3 py-1.5 text-sm transition-colors',
              activeFilter === 'warning'
                ? 'border border-amber-400 bg-amber-50 text-amber-700'
                : 'border border-gray-300 bg-gray-50 text-gray-700 hover:bg-gray-100',
            ]"
          >
            <AlertCircle class="h-4 w-4 text-amber-600" />
            <span class="text-sm font-medium">Avisos</span>
            <span class="rounded border border-gray-300 bg-white px-2 py-0.5 text-xs font-semibold">
              {{ displayWarning }}
            </span>
          </button>
        </div>
      </div>
    </div>

    <!-- LIST OF ALERTS -->
    <div class="space-y-4">
      <div
        v-for="a in filteredAlerts"
        :key="a.id"
        :class="[
          'flex items-start gap-4 rounded-lg p-5 shadow-lg',
          a.severity === 'critical'
            ? 'border border-red-200'
            : a.severity === 'warning'
            ? 'border border-amber-200'
            : 'border border-gray-200',
        ]"
        :style="
          a.severity === 'critical'
            ? { background: '#f6e3de', borderColor: '#dc2626' }
            : a.severity === 'warning'
            ? { background: '#fffbeb', borderColor: '#f59e0b' }
            : {}
        "
      >
        <!-- ICON -->
        <div class="flex-shrink-0">
          <component
            :is="a.severity === 'critical' ? AlertTriangle : AlertCircle"
            class="h-5 w-5"
            :class="[
              a.severity === 'critical'
                ? 'text-red-600'
                : a.severity === 'warning'
                ? 'text-amber-600'
                : 'text-slate-600',
            ]"
          />
        </div>

        <!-- CONTENT -->
        <div class="flex-1">
          <div class="flex items-start justify-between gap-3">
            <div>
              <div
                :class="[
                  'text-sm font-semibold',
                  a.severity === 'critical'
                    ? 'text-red-800'
                    : a.severity === 'warning'
                    ? 'text-amber-800'
                    : 'text-slate-800',
                ]"
              >
                {{ a.title }}
              </div>
              <div class="mt-1 text-xs text-gray-600">{{ a.subTitle }}</div>
            </div>

            <div class="flex-shrink-0">
              <span
                :class="[
                  'inline-block rounded-full px-3 py-1 text-xs text-white',
                  a.severity === 'critical'
                    ? 'bg-red-600'
                    : a.severity === 'warning'
                    ? 'bg-amber-600'
                    : 'bg-slate-600',
                ]"
              >
                {{ a.zone || a.tag }}
              </span>
            </div>
          </div>

          <div
            :class="[
              'mt-3 text-sm',
              a.severity === 'critical'
                ? 'text-red-700'
                : a.severity === 'warning'
                ? 'text-amber-700'
                : 'text-slate-700',
            ]"
          >
            {{ a.description }}
          </div>

          <!-- ACTION BUTTONS -->
          <div class="mt-4 flex items-center gap-3">
            <button
              @click="openModal(a, 'action')"
              class="inline-flex items-center gap-2 rounded-md px-3 py-1.5 text-sm font-medium"
              :style="
                a.severity === 'critical'
                  ? { background: '#dc2626', color: '#ffffff' }
                  : a.severity === 'warning'
                  ? { background: '#f59e0b', color: '#ffffff' }
                  : { background: '#374151', color: '#ffffff' }
              "
            >
              Agir
            </button>

            <button
              @click="openModal(a, 'details')"
              class="inline-flex items-center gap-2 rounded-md border border-gray-300 bg-white px-3 py-1.5 text-sm font-medium text-gray-700 transition-colors hover:bg-gray-50"
            >
              Ver detalhes
              <ExternalLink class="h-4 w-4" />
            </button>
          </div>
        </div>
      </div>
    </div>

    <!-- MODAL OVERLAY -->
    <div
      v-if="selectedAlert"
      class="fixed inset-0 z-50 flex items-center justify-center bg-black bg-opacity-50"
      @click="closeModal"
    >
      <div
        class="relative w-full rounded-xl bg-white shadow-2xl"
        :class="modalMode === 'action' ? 'max-w-2xl p-8' : 'max-w-lg p-6'"
        @click.stop
      >
        <!-- Close Button -->
        <button
          @click="closeModal"
          class="absolute right-4 top-4 text-gray-500 hover:text-gray-700"
        >
          <X class="h-5 w-5" />
        </button>

        <!-- ================= DETAILS MODE ================= -->
        <div v-if="modalMode === 'details'">
          <div class="mb-4 flex items-start gap-3">
            <component
              :is="selectedAlert.severity === 'critical' ? AlertTriangle : AlertCircle"
              class="h-6 w-6"
              :class="selectedAlert.severity === 'critical' ? 'text-red-600' : 'text-amber-600'"
            />

            <div>
              <h2 class="text-lg font-bold text-gray-900">
                {{ selectedAlert.title }}
              </h2>
              <p class="text-sm text-gray-500">
                {{ selectedAlert.subTitle }}
              </p>
            </div>
          </div>

          <div class="space-y-3 text-sm text-gray-700">
            <div>
              <span class="font-medium text-gray-900">Zona:</span>
              {{ selectedAlert.zone || '—' }}
            </div>

            <div>
              <span class="font-medium text-gray-900">Descrição:</span>
              {{ selectedAlert.description }}
            </div>

            <div>
              <span class="font-medium text-gray-900">Data:</span>
              {{ selectedAlert.date || new Date().toLocaleDateString('pt-PT') }}
            </div>
          </div>

          <div class="mt-6 flex justify-end">
            <button
              @click="closeModal"
              class="rounded-lg border border-gray-300 bg-white px-4 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50"
            >
              Fechar
            </button>
          </div>
        </div>

        <!-- ================= ACTION MODE ================= -->
        <div v-else>
          <!-- Header -->
          <div class="mb-6">
            <div class="flex items-center gap-3">
              <component
                :is="selectedAlert.severity === 'critical' ? AlertTriangle : AlertCircle"
                class="h-6 w-6"
                :class="selectedAlert.severity === 'critical' ? 'text-red-600' : 'text-amber-600'"
              />
              <div class="flex-1">
                <h2 class="text-2xl font-bold text-gray-900">
                  {{ selectedAlert.title }}
                </h2>
              </div>
              <span
                class="rounded-full px-3 py-1 text-xs font-semibold text-white"
                :class="selectedAlert.severity === 'critical' ? 'bg-red-600' : 'bg-amber-600'"
              >
                {{ selectedAlert.severity === 'critical' ? 'Crítico' : 'Aviso' }}
              </span>
            </div>
          </div>

          <!-- Body -->
          <div class="space-y-6 border-t border-gray-200 pt-6">
            <div>
              <h3 class="font-semibold text-gray-900">Descrição do Problema</h3>
              <p class="mt-2 text-sm text-gray-700">
                {{ getProblemDescription(selectedAlert) }}
              </p>
            </div>

            <div>
              <h3 class="font-semibold text-gray-900">Ações Recomendadas</h3>
              <div class="mt-3 space-y-2">
                <div
                  v-for="(action, idx) in getRecommendedActions(selectedAlert)"
                  :key="idx"
                  class="flex items-start gap-3 rounded-lg border border-gray-200 bg-gray-50 p-3"
                >
                  <div
                    class="mt-1 flex h-5 w-5 items-center justify-center rounded-full text-xs font-semibold text-white"
                    :class="selectedAlert.severity === 'critical' ? 'bg-red-600' : 'bg-amber-600'"
                  >
                    {{ idx + 1 }}
                  </div>
                  <p class="text-sm text-gray-700">{{ action }}</p>
                </div>
              </div>
            </div>
          </div>

          <!-- Footer -->
          <div class="mt-6 flex gap-3 border-t border-gray-200 pt-6">
            <button
              @click="handleResolve"
              class="flex-1 rounded-lg bg-red-600 px-4 py-2.5 text-sm font-medium text-white hover:bg-red-700"
            >
              Tomar Ação
            </button>
            <button
              @click="closeModal"
              class="flex-1 rounded-lg border border-gray-300 bg-white px-4 py-2.5 text-sm font-medium text-gray-700 hover:bg-gray-50"
            >
              Fechar
            </button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
  import { computed, ref, watch } from 'vue';
  import { AlertTriangle, AlertCircle, ExternalLink, X } from 'lucide-vue-next';
  import { useToast } from '../toaster/useToast';

  const { showToast } = useToast();

  const props = defineProps({
    alerts: { type: Array, required: true },
  });

  // State para guardar alertas resolvidos
  const resolvedAlertIds = ref(new Set());
  const selectedAlert = ref(null);
  const modalMode = ref(null);

  // Filtrar alertas que não foram resolvidos
  const activeAlerts = computed(() => {
    return props.alerts.filter((a) => !resolvedAlertIds.value.has(a.id));
  });

  const criticalCount = computed(
    () =>
      activeAlerts.value.filter((a) => ['critical'].includes(a.severity || a.level || a.type))
        .length
  );

  const warningCount = computed(
    () =>
      activeAlerts.value.filter((a) => ['warning'].includes(a.severity || a.level || a.type)).length
  );

  const displayCritical = computed(() => (criticalCount.value === 0 ? 0 : criticalCount.value));
  const displayWarning = computed(() => (warningCount.value === 0 ? 0 : warningCount.value));

  const activeFilter = ref('critical');

  const filteredAlerts = computed(() =>
    activeFilter.value === 'all'
      ? activeAlerts.value
      : activeAlerts.value.filter((a) => a.severity === activeFilter.value)
  );

  // Atualizar filtro se não houver alertas da categoria selecionada
  watch(activeAlerts, () => {
    if (activeFilter.value === 'critical' && criticalCount.value === 0 && warningCount.value > 0) {
      activeFilter.value = 'warning';
    }
  });

  function openModal(alert, mode) {
    selectedAlert.value = { ...alert };
    modalMode.value = mode;
  }

  function closeModal() {
    selectedAlert.value = null;
    modalMode.value = null;
  }

  function getProblemDescription(alert) {
    if (alert.title.includes('Ocupação')) {
      return 'A propriedade apresenta uma ocupação excessivamente elevada (>90% dos dias), o que pode indicar não conformidade com regulamentos locais ou falta de períodos de manutenção.';
    } else if (alert.title.includes('Preço')) {
      return 'O preço por noite está significativamente acima da média regional, o que pode afetar a competitividade ou indicar possível erro de configuração.';
    } else if (alert.title.includes('Avaliação')) {
      return 'A propriedade possui um número anormalmente elevado de avaliações, o que pode indicar atividade de replicação de listagens ou práticas de preços agressivas.';
    }
    return alert.description || 'Descrição não disponível.';
  }

  function getRecommendedActions(alert) {
    if (alert.title.includes('Ocupação')) {
      return [
        'Contatar o anfitrião para verificar conformidade com regulamentos de ocupação máxima',
        'Solicitar documentação de período de manutenção obrigatório',
        'Programar inspeção no local se necessário',
        'Considerar ajustar restrições de disponibilidade conforme necessário',
      ];
    } else if (alert.title.includes('Preço')) {
      return [
        'Comparar preço com outras propriedades similares na zona',
        'Contatar anfitrião para esclarecer motivo do preço elevado',
        'Verificar se o preço inclui serviços ou características especiais',
        'Solicitar ajuste se fora do mercado',
      ];
    } else if (alert.title.includes('Avaliação')) {
      return [
        'Revisar padrão de avaliações e datas para detectar anomalias',
        'Contatar anfitrião para explicação',
        'Verificar se múltiplas listagens referem propriedade idêntica',
        'Considerar suspensão temporária para investigação se necessário',
      ];
    }
    return [
      'Contactar proprietário para resolução',
      'Documentar ações tomadas',
      'Monitorizar situação',
      'Realizar follow-up em 7 dias',
    ];
  }

  function handleResolve() {
    if (selectedAlert.value && selectedAlert.value.id) {
      // Marcar como resolvido
      resolvedAlertIds.value.add(selectedAlert.value.id);

      // Notificar sucesso
      showToast('✅ Alerta resolvido com sucesso!');

      closeModal();
    }
  }
</script>

<style scoped></style>
