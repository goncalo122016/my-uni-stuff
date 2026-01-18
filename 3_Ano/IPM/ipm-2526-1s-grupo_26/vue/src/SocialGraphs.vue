<template>
  <div class="space-y-6">
    <!-- Social Header -->
    <SocialHeader
      title="Gráficos Sociais"
      description="Crie gráficos simples e impactantes otimizados para Instagram, Twitter e Facebook"
      primaryAction="Download All"
      secondaryAction="Partilhar coleção"
      tertiaryAction="Copiar link"
      @download-all="handleDownloadAll"
      @share-collection="handleShareCollection"
      @copy-link="handleCopyCollectionLink"
    />

    <!-- KPI Grid -->
    <KpiGrid ref="kpiGridRef" />

    <!-- Social Graphics Section -->
    <div class="rounded-xl border border-gray-300 bg-white p-6 shadow-lg">
      <div class="mb-6">
        <h3 class="text-lg font-bold text-gray-900">Gráficos Prontos para Partilhar</h3>
        <p class="mt-1 text-sm text-gray-500">
          Descarregue e partilhe nos seus canais de comunicação social
        </p>
      </div>

      <!-- Graphics Grid -->
      <div class="grid grid-cols-1 gap-6 md:grid-cols-2 lg:grid-cols-3">
        <!-- Chart 1: Listings by Type -->
        <div
          class="rounded-lg border border-gray-200 bg-gradient-to-br from-blue-50 to-blue-100 p-4 shadow"
        >
          <div class="mb-4 flex items-center justify-between">
            <h4 class="font-semibold text-gray-900">Listagens por Tipo</h4>
            <span class="rounded-full bg-blue-200 px-2 py-1 text-xs font-semibold text-blue-700"
              >PNG</span
            >
          </div>
          <div class="mb-4 flex h-40 justify-center rounded bg-white p-3">
            <canvas ref="chartListingsType"></canvas>
          </div>
          <div class="flex justify-center">
            <button
              @click="downloadChartImage('chartListingsType', 'Listagens-por-Tipo')"
              class="flex items-center gap-2 rounded bg-blue-600 px-6 py-2 text-sm font-medium text-white hover:bg-blue-700"
            >
              <Download class="h-4 w-4" />
              Descarregar
            </button>
          </div>
        </div>

        <!-- Chart 2: Price Distribution -->
        <div
          class="rounded-lg border border-gray-200 bg-gradient-to-br from-green-50 to-green-100 p-4 shadow"
        >
          <div class="mb-4 flex items-center justify-between">
            <h4 class="font-semibold text-gray-900">Distribuição de Preços</h4>
            <span class="rounded-full bg-green-200 px-2 py-1 text-xs font-semibold text-green-700"
              >PNG</span
            >
          </div>
          <div class="mb-4 flex h-40 justify-center rounded bg-white p-3">
            <canvas ref="chartPriceDistribution"></canvas>
          </div>
          <div class="flex justify-center">
            <button
              @click="downloadChartImage('chartPriceDistribution', 'Distribuicao-Precos')"
              class="flex items-center gap-2 rounded bg-green-600 px-6 py-2 text-sm font-medium text-white hover:bg-green-700"
            >
              <Download class="h-4 w-4" />
              Descarregar
            </button>
          </div>
        </div>

        <!-- Chart 3: Timeline Trend -->
        <div
          class="rounded-lg border border-gray-200 bg-gradient-to-br from-purple-50 to-purple-100 p-4 shadow"
        >
          <div class="mb-4 flex items-center justify-between">
            <h4 class="font-semibold text-gray-900">Tendência Temporal</h4>
            <span class="rounded-full bg-purple-200 px-2 py-1 text-xs font-semibold text-purple-700"
              >PNG</span
            >
          </div>
          <div class="mb-4 flex h-40 justify-center rounded bg-white p-3">
            <canvas ref="chartTimelineTrend"></canvas>
          </div>
          <div class="flex justify-center">
            <button
              @click="downloadChartImage('chartTimelineTrend', 'Tendencia-Temporal')"
              class="flex items-center gap-2 rounded bg-purple-600 px-6 py-2 text-sm font-medium text-white hover:bg-purple-700"
            >
              <Download class="h-4 w-4" />
              Descarregar
            </button>
          </div>
        </div>

        <!-- Chart 4: Occupancy Rate -->
        <div
          class="rounded-lg border border-gray-200 bg-gradient-to-br from-orange-50 to-orange-100 p-4 shadow"
        >
          <div class="mb-4 flex items-center justify-between">
            <h4 class="font-semibold text-gray-900">Taxa de Ocupação</h4>
            <span class="rounded-full bg-orange-200 px-2 py-1 text-xs font-semibold text-orange-700"
              >PNG</span
            >
          </div>
          <div class="mb-4 flex h-40 justify-center rounded bg-white p-3">
            <canvas ref="chartOccupancyRate"></canvas>
          </div>
          <div class="flex justify-center">
            <button
              @click="downloadChartImage('chartOccupancyRate', 'Taxa-Ocupacao')"
              class="flex items-center gap-2 rounded bg-orange-600 px-6 py-2 text-sm font-medium text-white hover:bg-orange-700"
            >
              <Download class="h-4 w-4" />
              Descarregar
            </button>
          </div>
        </div>

        <!-- Chart 5: Neighborhoods -->
        <div
          class="rounded-lg border border-gray-200 bg-gradient-to-br from-pink-50 to-pink-100 p-4 shadow"
        >
          <div class="mb-4 flex items-center justify-between">
            <h4 class="font-semibold text-gray-900">Zonas Principais</h4>
            <span class="rounded-full bg-pink-200 px-2 py-1 text-xs font-semibold text-pink-700"
              >PNG</span
            >
          </div>
          <div class="mb-4 flex h-40 justify-center rounded bg-white p-3">
            <canvas ref="chartNeighborhoods"></canvas>
          </div>
          <div class="flex justify-center">
            <button
              @click="downloadChartImage('chartNeighborhoods', 'Zonas-Principais')"
              class="flex items-center gap-2 rounded bg-pink-600 px-6 py-2 text-sm font-medium text-white hover:bg-pink-700"
            >
              <Download class="h-4 w-4" />
              Descarregar
            </button>
          </div>
        </div>

        <!-- Chart 6: Summary Stats -->
        <div
          class="rounded-lg border border-gray-200 bg-gradient-to-br from-teal-50 to-teal-100 p-4 shadow"
        >
          <div class="mb-4 flex items-center justify-between">
            <h4 class="font-semibold text-gray-900">Resumo de Estatísticas</h4>
            <span class="rounded-full bg-teal-200 px-2 py-1 text-xs font-semibold text-teal-700"
              >PNG</span
            >
          </div>
          <div class="mb-4 flex h-40 justify-center rounded bg-white p-3">
            <canvas ref="chartSummaryStats"></canvas>
          </div>
          <div class="flex justify-center">
            <button
              @click="downloadChartImage('chartSummaryStats', 'Resumo-Estatisticas')"
              class="flex items-center gap-2 rounded bg-teal-600 px-6 py-2 text-sm font-medium text-white hover:bg-teal-700"
            >
              <Download class="h-4 w-4" />
              Descarregar
            </button>
          </div>
        </div>
      </div>
    </div>
  </div>

  <!-- Share Modal -->
  <div
    v-if="showShareModal"
    class="fixed inset-0 z-50 flex items-center justify-center bg-black/40"
  >
    <div class="w-full max-w-sm rounded-xl bg-white p-6 shadow-xl">
      <h3 class="mb-4 text-lg font-bold text-gray-900">Partilhar coleção</h3>

      <div class="grid grid-cols-3 gap-6">
        <button
          v-for="option in shareOptions"
          :key="option.name"
          @click="option.action"
          class="flex flex-col items-center gap-2 rounded-lg p-4 transition hover:bg-gray-100"
        >
          <img :src="option.image" :alt="option.name" class="h-12 w-12 object-contain" />
          <span class="text-sm font-medium text-gray-700">
            {{ option.name }}
          </span>
        </button>
      </div>

      <div class="mt-6 flex justify-end">
        <button
          @click="showShareModal = false"
          class="text-sm font-medium text-gray-500 hover:text-gray-700"
        >
          Cancelar
        </button>
      </div>
    </div>
  </div>
</template>

<script setup>
  import { ref, onMounted, watch } from 'vue';
  import Chart from 'chart.js/auto';
  import KpiGrid from './components/social/KpiGrid.vue';
  import SocialHeader from './components/social/SocialHeader.vue';
  import { Download } from 'lucide-vue-next';
  import { useToast } from './components/toaster/useToast';
  import { loadCityData } from '../data/backend';
  import { useInsideAirbnbStore } from './stores/insideAirbnbStore';
  import whatsappImg from './assets/whatsapp.png';
  import discordImg from './assets/discord.png';
  import gmailImg from './assets/gmail.png';

  const { showToast } = useToast();
  const insideAirbnbStore = useInsideAirbnbStore();

  /* Chart refs  */
  const chartListingsType = ref(null);
  const chartPriceDistribution = ref(null);
  const chartTimelineTrend = ref(null);
  const chartOccupancyRate = ref(null);
  const chartNeighborhoods = ref(null);
  const chartSummaryStats = ref(null);

  const kpiGridRef = ref(null);

  const chartInstances = {};

  const showShareModal = ref(false);
  const shareOptions = ref([]);

  onMounted(async () => {
    initializeCharts();
    await updateCharts();
  });

  watch(
    () => [insideAirbnbStore.selectedCity, insideAirbnbStore.period],
    () => {
      updateCharts();
    }
  );

  function initializeCharts() {
    if (chartListingsType.value) {
      chartInstances.listingsType = new Chart(chartListingsType.value, {
        type: 'bar',
        data: {
          labels: ['Casa Inteira', 'Quarto Privado', 'Quarto Partilhado', 'Hotel Room'],
          datasets: [
            {
              data: [0, 0, 0, 0],
              backgroundColor: ['#3b82f6', '#2563eb', '#1d4ed8', '#1e40af'],
              borderRadius: 4,
            },
          ],
        },
        options: {
          plugins: { legend: { display: false } },
          scales: { y: { beginAtZero: true } },
        },
      });
    }

    if (chartPriceDistribution.value) {
      chartInstances.priceDistribution = new Chart(chartPriceDistribution.value, {
        type: 'doughnut',
        data: {
          labels: ['€0-50', '€50-100', '€100-200', '€200+'],
          datasets: [
            {
              data: [0, 0, 0, 0],
              backgroundColor: ['#10b981', '#34d399', '#6ee7b7', '#a7f3d0'],
            },
          ],
        },
      });
    }

    if (chartTimelineTrend.value) {
      chartInstances.timelineTrend = new Chart(chartTimelineTrend.value, {
        type: 'line',
        data: {
          labels: ['Jan', 'Fev', 'Mar'],
          datasets: [
            {
              label: 'Variação da Média',
              data: [0, 0, 0],
              borderColor: '#a855f7',
              backgroundColor: 'rgba(168,85,247,0.1)',
              fill: true,
              tension: 0.4,
            },
          ],
        },
        options: {
          plugins: {
            legend: { display: false },
            tooltip: {
              callbacks: {
                label: function (context) {
                  const value = context.parsed.y;
                  const sign = value >= 0 ? '+' : '';
                  return sign + value.toFixed(2);
                },
              },
            },
          },
          scales: {
            y: {
              beginAtZero: true,
              ticks: {
                callback: function (value) {
                  const sign = value >= 0 ? '+' : '';
                  return sign + value.toFixed(2);
                },
              },
            },
          },
        },
      });
    }

    if (chartOccupancyRate.value) {
      chartInstances.occupancyRate = new Chart(chartOccupancyRate.value, {
        type: 'doughnut',
        data: {
          labels: ['Ocupado', 'Disponível'],
          datasets: [
            {
              data: [0, 100],
              backgroundColor: ['#ea580c', '#fed7aa'],
            },
          ],
        },
      });
    }

    if (chartNeighborhoods.value) {
      chartInstances.neighborhoods = new Chart(chartNeighborhoods.value, {
        type: 'bar',
        data: {
          labels: ['Zona 1', 'Zona 2', 'Zona 3', 'Zona 4', 'Zona 5'],
          datasets: [
            {
              data: [0, 0, 0, 0, 0],
              backgroundColor: '#ec4899',
              borderRadius: 4,
            },
          ],
        },
        options: {
          indexAxis: 'y',
          plugins: { legend: { display: false } },
        },
      });
    }

    if (chartSummaryStats.value) {
      chartInstances.summaryStats = new Chart(chartSummaryStats.value, {
        type: 'radar',
        data: {
          labels: ['Listagens', 'Preço', 'Ocupação', 'Reviews', 'Anfitriões'],
          datasets: [
            {
              label: insideAirbnbStore.cityLabel,
              data: [0, 0, 0, 0, 0],
              borderColor: '#14b8a6',
              backgroundColor: 'rgba(20,184,166,0.2)',
            },
          ],
        },
      });
    }
  }

  async function updateCharts() {
    const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);

    if (!data || !data.listings || data.listings.length === 0) {
      console.warn('Sem dados disponíveis');
      return;
    }

    const listings = data.listings || [];
    const reviews = data.reviews || [];

    const typeCount = listings.reduce((acc, listing) => {
      const type = listing.room_type || 'Unknown';
      acc[type] = (acc[type] || 0) + 1;
      return acc;
    }, {});

    if (chartInstances.listingsType) {
      chartInstances.listingsType.data.datasets[0].data = Object.values(typeCount);
      chartInstances.listingsType.data.labels = Object.keys(typeCount);
      chartInstances.listingsType.update();
    }

    const priceRanges = {
      '€0-50': 0,
      '€50-100': 0,
      '€100-200': 0,
      '€200+': 0,
    };

    listings.forEach((listing) => {
      const price = listing.price || 0;
      if (price <= 50) priceRanges['€0-50']++;
      else if (price <= 100) priceRanges['€50-100']++;
      else if (price <= 200) priceRanges['€100-200']++;
      else priceRanges['€200+']++;
    });

    if (chartInstances.priceDistribution) {
      chartInstances.priceDistribution.data.datasets[0].data = Object.values(priceRanges);
      chartInstances.priceDistribution.update();
    }

    if (chartInstances.timelineTrend) {
      const period = insideAirbnbStore.period;
      const monthsData = getQuadrimesterMonthsData(period, reviews, listings);

      chartInstances.timelineTrend.data.labels = monthsData.labels;
      chartInstances.timelineTrend.data.datasets[0].data = monthsData.values;
      chartInstances.timelineTrend.update();
    }

    const occupiedListings = new Set(reviews.map((r) => r.listing_id)).size;
    const occupancyRate = listings.length > 0 ? (occupiedListings / listings.length) * 100 : 0;

    if (chartInstances.occupancyRate) {
      chartInstances.occupancyRate.data.datasets[0].data = [
        Math.round(occupancyRate),
        Math.round(100 - occupancyRate),
      ];
      chartInstances.occupancyRate.update();
    }

    const neighbourhoodCount = listings.reduce((acc, listing) => {
      const neighbourhood = listing.neighbourhood || 'Unknown';
      acc[neighbourhood] = (acc[neighbourhood] || 0) + 1;
      return acc;
    }, {});

    const topNeighbourhoods = Object.entries(neighbourhoodCount)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5)
      .reduce((acc, [name, count]) => {
        acc[name] = count;
        return acc;
      }, {});

    if (chartInstances.neighborhoods) {
      chartInstances.neighborhoods.data.datasets[0].data = Object.values(topNeighbourhoods);
      chartInstances.neighborhoods.data.labels = Object.keys(topNeighbourhoods);
      chartInstances.neighborhoods.update();
    }

    const avgPrice =
      listings.length > 0
        ? listings.reduce((sum, l) => sum + (l.price || 0), 0) / listings.length
        : 0;
    const avgReviews =
      listings.length > 0
        ? listings.reduce((sum, l) => sum + (l.number_of_reviews || 0), 0) / listings.length
        : 0;
    const uniqueHosts = new Set(listings.map((l) => l.host_id)).size;

    if (chartInstances.summaryStats) {
      chartInstances.summaryStats.data.datasets[0].data = [
        Math.min(100, (listings.length / 3000) * 100),
        Math.min(100, (avgPrice / 300) * 100),
        Math.min(100, occupancyRate),
        Math.min(100, (avgReviews / 100) * 100),
        Math.min(100, (uniqueHosts / listings.length) * 100),
      ];
      chartInstances.summaryStats.update();
    }
  }

  function getQuadrimesterMonthsData(period, reviews, listings) {
    const monthNames = [
      'Jan',
      'Fev',
      'Mar',
      'Abr',
      'Mai',
      'Jun',
      'Jul',
      'Ago',
      'Set',
      'Out',
      'Nov',
      'Dez',
    ];

    // Extrair quadrimestre do período
    const match = period.match(/Q(\d)\s+(\d{4})/);
    if (!match) {
      return { labels: ['Mês 1', 'Mês 2', 'Mês 3'], values: [0, 0, 0] };
    }

    const q = parseInt(match[1]);
    const year = parseInt(match[2]);

    // Determinar meses do quadrimestre (3 meses cada)
    let startMonth, endMonth;
    switch (q) {
      case 1:
        startMonth = 1;
        endMonth = 3;
        break; // Jan-Mar
      case 2:
        startMonth = 4;
        endMonth = 6;
        break; // Abr-Jun
      case 3:
        startMonth = 7;
        endMonth = 9;
        break; // Jul-Set
      case 4:
        startMonth = 10;
        endMonth = 12;
        break; // Out-Dez
      default:
        startMonth = 1;
        endMonth = 3;
    }

    const labels = [];
    const rawValues = [];

    // Se houver reviews, contar por mês
    if (reviews && reviews.length > 0) {
      for (let month = startMonth; month <= endMonth; month++) {
        labels.push(monthNames[month - 1]);

        const reviewCount = reviews.filter((review) => {
          if (!review.date) return false;
          const reviewDate = new Date(review.date);
          return reviewDate.getMonth() + 1 === month && reviewDate.getFullYear() === year;
        }).length;

        rawValues.push(reviewCount);
      }
    } else {
      // Fallback: contar listagens por mês baseado em last_review
      for (let month = startMonth; month <= endMonth; month++) {
        labels.push(monthNames[month - 1]);

        const listingCount = listings.filter((listing) => {
          if (!listing.last_review) return false;
          const reviewDate = new Date(listing.last_review);
          return reviewDate.getMonth() + 1 === month;
        }).length;

        rawValues.push(listingCount);
      }
    }

    // Se não houver dados, usar distribuição variada
    if (rawValues.every((v) => v === 0) && listings.length > 0) {
      rawValues[0] = 80;
      rawValues[1] = 100;
      rawValues[2] = 120;
    }

    // Calcular média dos 3 meses
    const total = rawValues.reduce((sum, val) => sum + val, 0);
    const average = total / 3;

    // Converter para fração relativa à média
    // Exemplo: 90 quando média é 100 → (90/100 - 1) = -0.1
    //          110 quando média é 100 → (110/100 - 1) = +0.1
    const values =
      average > 0 ? rawValues.map((val) => parseFloat((val / average - 1).toFixed(2))) : [0, 0, 0];

    return { labels, values };
  }

  function downloadChartImage(refName, fileName) {
    const map = {
      chartListingsType,
      chartPriceDistribution,
      chartTimelineTrend,
      chartOccupancyRate,
      chartNeighborhoods,
      chartSummaryStats,
    };

    const canvas = map[refName]?.value;
    if (!canvas) return;

    canvas.toBlob((blob) => {
      const url = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = `${fileName}.png`;
      link.click();
      URL.revokeObjectURL(url);
    });
  }

  function handleDownloadAll() {
    const charts = [
      ['chartListingsType', 'Listagens-por-Tipo'],
      ['chartPriceDistribution', 'Distribuicao-Precos'],
      ['chartTimelineTrend', 'Tendencia-Temporal'],
      ['chartOccupancyRate', 'Taxa-Ocupacao'],
      ['chartNeighborhoods', 'Zonas-Principais'],
      ['chartSummaryStats', 'Resumo-Estatisticas'],
    ];

    charts.forEach(([ref, name], i) => setTimeout(() => downloadChartImage(ref, name), i * 400));

    setTimeout(() => {
      kpiGridRef.value?.downloadAllCards();
    }, charts.length * 300);

    showToast('A descarregar todos os gráficos');
  }

  function handleCopyCollectionLink() {
    const shareLink = generateShareLink();
    navigator.clipboard.writeText(shareLink);
    showToast('Link copiado para a área de transferência');
  }

  function generateShareLink() {
    const baseUrl = window.location.origin + window.location.pathname;
    const params = new URLSearchParams({
      city: insideAirbnbStore.selectedCity,
      period: insideAirbnbStore.period,
    });
    return `${baseUrl}?${params.toString()}`;
  }

  function handleShareCollection() {
    const link = generateShareLink();

    shareOptions.value = [
      {
        name: 'WhatsApp',
        image: whatsappImg,
        action: () => {
          window.open(
            `https://wa.me/?text=${encodeURIComponent(
              'Dá uma vista de olhos a estes dados:\n' + link
            )}`,
            '_blank'
          );
          showToast('A abrir WhatsApp');
        },
      },
      {
        name: 'Discord',
        image: discordImg,
        action: () => {
          const discordUrl = `https://discord.com/channels/`;

          window.open(discordUrl, '_blank');
          showToast('A abrir Discord');
        },
      },
      {
        name: 'Email (Gmail)',
        image: gmailImg,
        action: () => {
          const gmailUrl =
            `https://mail.google.com/mail/?view=cm&fs=1` +
            `&su=${encodeURIComponent('Relatório Airbnb')}` +
            `&body=${encodeURIComponent('Olá!\n\nPartilho contigo este relatório:\n\n' + link)}`;

          window.open(gmailUrl, '_blank');
          showToast('A abrir Gmail');
        },
      },
    ];

    showShareModal.value = true;
  }
</script>

<style scoped></style>
