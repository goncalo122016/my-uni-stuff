<script setup>
  import { ref, onMounted, watch } from 'vue';
  import {
    countAllListings,
    exportCityData,
    exportCityDataByPeriod,
    exportPriceAnalysis,
  } from '../data/backend.js';
  import {
    Database,
    Calendar,
    FileSpreadsheet,
    FileJson,
    ChevronDown,
    Download,
  } from 'lucide-vue-next';
  import { useToast } from './components/toaster/useToast.js';
  import { useInsideAirbnbStore } from './stores/insideAirbnbStore';

  const { showToast } = useToast();
  const insideAirbnbStore = useInsideAirbnbStore();

  function exportPriceAnalysisData() {
    exportPriceAnalysis(insideAirbnbStore.selectedCity);
    showToast(`✅ Exportação de análise de preço iniciada em formato XLSX`);
  }

  function exportData(format) {
    exportCityData(insideAirbnbStore.selectedCity, format);
    showToast(`✅ Exportação de dados iniciada em formato ${format.toUpperCase()}`);
  }

  function exportDataByPeriod(format) {
    if (!formatOption.value) {
      showToast('⚠️ Por favor, selecione um formato de exportação');
      return;
    }
    console.log(
      `📥 Exportando dados de ${insideAirbnbStore.selectedCity} em formato ${format} para período ${insideAirbnbStore.period}`
    );
    exportCityDataByPeriod(insideAirbnbStore.selectedCity, format, insideAirbnbStore.period);
  }

  const quadrimestre = {
    'Q1 2024': '1º Quadrimestre 2024',
    'Q2 2024': '2º Quadrimestre 2024',
    'Q3 2024': '3º Quadrimestre 2024',
    'Q4 2024': '4º Quadrimestre 2024',
  };

  const formatOption = ref('');

  const totalListings = ref(null);

  const showFormatDropdown = ref(false);

  const formats = [
    { value: 'csv', label: 'CSV', icon: FileSpreadsheet },
    { value: 'json', label: 'JSON', icon: FileJson },
    { value: 'xlsx', label: 'XLSX', icon: FileSpreadsheet },
  ];

  async function loadListingsCount() {
    totalListings.value = await countAllListings(insideAirbnbStore.selectedCity);
  }

  onMounted(loadListingsCount);
  watch(() => insideAirbnbStore.selectedCity, loadListingsCount);
  watch(() => insideAirbnbStore.period, loadListingsCount);
</script>

<template>
  <div class="grid grid-cols-1 gap-8 lg:grid-cols-[2fr_1fr]">
    <div class="space-y-6">
      <!-- Exportar Dados Section -->
      <div class="rounded-xl border border-gray-200 bg-white p-6 shadow">
        <h2 class="mb-1 text-xl font-bold">Exportar Dados</h2>
        <p class="mb-6 text-sm text-gray-600">Exporte dados em vários formatos para análise</p>
        <div class="flex items-center gap-8">
          <div class="flex items-center gap-2">
            <Database class="text-gray-500" />
            <span class="text-2xl font-semibold">
              {{ totalListings ?? '...' }}
            </span>
            <span class="text-sm text-gray-600">alojamentos</span>
          </div>
          <div class="flex items-center gap-2">
            <Calendar class="text-gray-500" />
            <span class="text-2xl font-semibold">{{ insideAirbnbStore.period }}</span>
            <span class="text-sm text-gray-600">Período atual</span>
          </div>
        </div>
      </div>
      <!-- Opções de Exportação Section -->
      <div class="rounded-xl border border-gray-200 bg-white p-6 shadow">
        <h2 class="mb-1 text-xl font-bold">Opções de Exportação</h2>
        <p class="mb-6 text-sm text-gray-600">Configure as definições de exportação</p>
        <label class="mb-2 block text-sm font-medium text-gray-700">Formato</label>
        <div class="relative mb-6">
          <button
            @click="showFormatDropdown = !showFormatDropdown"
            class="flex w-full items-center justify-between rounded-lg border border-gray-200 bg-gray-50 p-2 hover:bg-gray-100"
          >
            <div class="flex items-center gap-2">
              <component
                v-if="formatOption"
                :is="formats.find((f) => f.value === formatOption)?.icon"
                class="h-4 w-4 text-gray-600"
              />
              <span class="text-gray-700">
                {{
                  formatOption
                    ? formats.find((f) => f.value === formatOption)?.label
                    : 'Selecione um formato'
                }}
              </span>
            </div>
            <ChevronDown class="h-4 w-4 text-gray-600" />
          </button>

          <div
            v-if="showFormatDropdown"
            class="absolute left-0 right-0 top-full z-50 mt-2 rounded-lg border border-gray-200 bg-white shadow-lg"
          >
            <button
              v-for="format in formats"
              :key="format.value"
              @click="
                formatOption = format.value;
                showFormatDropdown = false;
              "
              class="flex w-full items-center gap-3 px-4 py-2 text-left hover:bg-gray-50"
              :class="formatOption === format.value ? 'bg-gray-100' : ''"
            >
              <component :is="format.icon" class="h-4 w-4 text-gray-600" />
              <span class="flex-1">{{ format.label }}</span>
              <span v-if="formatOption === format.value" class="h-4 w-4 text-primary">✓</span>
            </button>
          </div>
        </div>
        <label class="mb-2 block text-sm font-medium text-gray-700">Cidade</label>
        <div class="mb-6 w-full rounded-lg border-gray-300 bg-gray-200 p-2">
          <span class="capitalize">{{
            insideAirbnbStore.selectedCity.replace('-', ' ') === 'Lisbon'
              ? 'Lisboa'
              : insideAirbnbStore.selectedCity.replace('-', ' ')
          }}</span>
        </div>
        <label class="mb-2 block text-sm font-medium text-gray-700">Período Temporal</label>
        <div class="mb-6 w-full rounded-lg border-gray-300 bg-gray-200 p-2">
          <span class="">{{ quadrimestre[insideAirbnbStore.period] }}</span>
        </div>
        <button
          class="flex w-full items-center justify-center gap-2 rounded-lg bg-primary py-2 text-sm font-medium text-white"
          @click="exportDataByPeriod(formatOption)"
        >
          <Download class="h-5 w-5" />
          Exportar Dados
        </button>
        <p class="mt-4 text-center text-xs text-gray-600">
          {{ totalListings ?? '...' }} linhas com todos os campos
        </p>
      </div>
    </div>
    <div class="rounded-xl border border-gray-200 bg-white p-6 shadow">
      <h2 class="mb-1 text-xl font-bold">Exportações Rápidas</h2>
      <p class="mb-6 text-sm text-gray-600">Modelos de exportação pré-configurados</p>
      <div class="space-y-3">
        <button
          class="flex w-full items-center justify-between rounded-lg bg-gray-100 p-3 transition hover:bg-gray-200"
          @click="exportData('csv')"
        >
          <FileSpreadsheet />
          <span>Dados Básicos (CSV)</span>
          <span class="text-sm text-gray-600">CSV</span>
        </button>

        <button
          class="flex w-full items-center justify-between rounded-lg bg-gray-100 p-3 transition hover:bg-gray-200"
          @click="exportData('json')"
        >
          <FileJson />
          <span>Dataset Completo (JSON)</span>
          <span class="text-sm text-gray-600">JSON</span>
        </button>
        <button
          class="flex w-full items-center justify-between rounded-lg bg-gray-100 p-3 transition hover:bg-gray-200"
          @click="exportPriceAnalysisData()"
        >
          <FileSpreadsheet />
          <span>Análise de Preço (XSLX)</span>
          <span class="text-sm text-gray-600">XSLX</span>
        </button>
      </div>
    </div>
  </div>
</template>
