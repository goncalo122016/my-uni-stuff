<template>
  <div class="mb-8 grid grid-cols-1 gap-6 lg:grid-cols-2">
    <!-- Room Type Chart -->
    <DownloadableCard fileName="Listagens_por_Tipo">
      <div data-chart-container data-chart-title="Listagens por Tipo" class="bg-white">
        <div class="mb-6">
          <h3 class="text-lg font-bold text-gray-900">Listagens por Tipo</h3>
          <p class="mt-1 text-sm text-gray-500">
            Distribuição de listagens por tipo em {{ insideAirbnbStore.selectedCity }}
          </p>
        </div>

        <div class="h-72">
          <canvas ref="roomTypeChart"></canvas>
        </div>
      </div>
    </DownloadableCard>

    <!-- Price Range Chart -->
    <DownloadableCard fileName="Distribuicao_de_Precos">
      <div data-chart-container data-chart-title="Distribuição de Preços" class="bg-white">
        <div class="mb-6">
          <h3 class="text-lg font-bold text-gray-900">Distribuição de Preços</h3>
          <p class="mt-1 text-sm text-gray-500">Preço por noite em todas as listagens (€)</p>
        </div>

        <div class="h-72">
          <canvas ref="priceRangeChart"></canvas>
        </div>
      </div>
    </DownloadableCard>
  </div>
</template>

<script setup>
  import { ref, watch, onMounted, nextTick } from 'vue';
  import Chart from 'chart.js/auto';
  import { loadCityData, groupByRoomType, groupByPriceRange } from '../../../data/backend.js';
  import DownloadableCard from '../DownloadableCard.vue';
  import { useInsideAirbnbStore } from '../../stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();

  const roomTypeChart = ref(null);
  const priceRangeChart = ref(null);
  let roomTypeInstance = null;
  let priceRangeInstance = null;

  async function loadCharts() {
    const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);

    if (!data || !data.listings || data.listings.length === 0) {
      return;
    }

    // Processar dados de tipo de quarto
    const roomTypeData = groupByRoomType(data);
    const roomTypes = Object.keys(roomTypeData);
    const roomTypeCounts = Object.values(roomTypeData);

    // Processar dados de faixa de preço
    const priceRangeData = groupByPriceRange(data);
    const priceLabels = ['0-50€', '50-100€', '100-200€', '200-300€', '300+€'];
    const priceCounts = [
      priceRangeData['0-50'] || 0,
      priceRangeData['50-100'] || 0,
      priceRangeData['100-200'] || 0,
      priceRangeData['200-300'] || 0,
      priceRangeData['300+'] || 0,
    ];

    await nextTick();
    renderRoomTypeChart(roomTypes, roomTypeCounts);
    renderPriceChart(priceLabels, priceCounts);
  }

  function renderRoomTypeChart(labels, data) {
    const ctx = roomTypeChart.value?.getContext('2d');
    if (!ctx) return;

    if (roomTypeInstance) {
      roomTypeInstance.destroy();
    }

    const colors = ['#111827', '#374151', '#6b7280', '#9ca3af'];

    roomTypeInstance = new Chart(ctx, {
      type: 'bar',
      data: {
        labels: labels,
        datasets: [
          {
            label: 'Número de Listagens',
            data: data,
            backgroundColor: colors.slice(0, labels.length),
            borderRadius: 6,
            borderSkipped: false,
          },
        ],
      },
      options: {
        indexAxis: 'y',
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: {
            display: true,
            position: 'top',
            labels: {
              color: '#6b7280',
              font: { size: 12 },
              usePointStyle: true,
              padding: 15,
            },
          },
        },
        scales: {
          x: {
            beginAtZero: true,
            ticks: {
              color: '#9ca3af',
              font: { size: 11 },
            },
            grid: {
              color: 'rgba(229, 231, 235, 0.5)',
              drawBorder: false,
            },
          },
          y: {
            ticks: {
              color: '#9ca3af',
              font: { size: 11 },
            },
            grid: {
              display: false,
              drawBorder: false,
            },
          },
        },
      },
    });
  }

  function renderPriceChart(labels, data) {
    const ctx = priceRangeChart.value?.getContext('2d');
    if (!ctx) return;

    if (priceRangeInstance) {
      priceRangeInstance.destroy();
    }

    priceRangeInstance = new Chart(ctx, {
      type: 'bar',
      data: {
        labels: labels,
        datasets: [
          {
            label: 'Número de Listagens',
            data: data,
            backgroundColor: [
              'rgba(17, 24, 39, 0.8)',
              'rgba(17, 24, 39, 0.7)',
              'rgba(17, 24, 39, 0.6)',
              'rgba(17, 24, 39, 0.5)',
              'rgba(17, 24, 39, 0.4)',
            ],
            borderRadius: 6,
            borderSkipped: false,
          },
        ],
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: {
            display: true,
            position: 'top',
            labels: {
              color: '#6b7280',
              font: { size: 12 },
              usePointStyle: true,
              padding: 15,
            },
          },
        },
        scales: {
          y: {
            beginAtZero: true,
            ticks: {
              color: '#9ca3af',
              font: { size: 11 },
            },
            grid: {
              color: 'rgba(229, 231, 235, 0.5)',
              drawBorder: false,
            },
          },
          x: {
            ticks: {
              color: '#9ca3af',
              font: { size: 11 },
            },
            grid: {
              display: false,
              drawBorder: false,
            },
          },
        },
      },
    });
  }

  onMounted(loadCharts);
  watch(() => insideAirbnbStore.selectedCity, loadCharts);
  watch(() => insideAirbnbStore.period, loadCharts);
</script>

<style scoped></style>
