<template>
  <div class="rounded-xl border border-gray-200 bg-white p-6 shadow-lg">
    <h2 class="mb-4 text-lg font-bold text-gray-900">Estatísticas</h2>
    <div class="space-y-4">
      <div class="flex gap-4">
        <div class="flex-1">
          <div class="mb-1 text-xs font-medium text-gray-600">Listagens:</div>
          <div class="text-2xl font-bold text-[#0080a1]">{{ totalListings.toLocaleString() }}</div>
        </div>
        <div class="flex-1">
          <div class="mb-1 text-xs font-medium text-gray-600">Preço médio:</div>
          <div class="text-2xl font-bold text-green-600">€{{ averagePrice }}/noite</div>
        </div>
        <div class="flex-1">
          <div class="mb-1 text-xs font-medium text-gray-600">Densidade:</div>
          <div class="text-2xl font-bold text-blue-600">{{ density }}</div>
        </div>
      </div>
      <div class="mt-4 border-t border-gray-200 pt-4">
        <h3 class="mb-3 text-xs font-semibold text-gray-700">Áreas principais</h3>
        <div class="space-y-2 text-xs">
          <div v-for="(area, idx) in topAreas" :key="idx" class="flex justify-between">
            <span class="text-gray-600">{{ area.name }}</span>
            <span class="font-medium text-gray-900">{{ area.count.toLocaleString() }}</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
  import { ref, watch, onMounted } from 'vue';
  import { loadCityData, calculateKPIs, groupByNeighbourhood } from '../../../data/backend.js';
  import { useInsideAirbnbStore } from '../../stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();

  const totalListings = ref(0);
  const averagePrice = ref(0);
  const density = ref('Média');
  const topAreas = ref([]);

  async function loadStats() {
    const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);

    if (!data || !data.listings || data.listings.length === 0) {
      totalListings.value = 0;
      averagePrice.value = 0;
      density.value = 'Baixa';
      topAreas.value = [];
      return;
    }

    // Calcular KPIs
    const kpis = calculateKPIs(data);
    totalListings.value = kpis.totalListings;
    averagePrice.value = Math.round(kpis.avgPrice);

    // Densidade baseada na média de preço
    if (kpis.avgPrice > 200) {
      density.value = 'Alta';
    } else if (kpis.avgPrice > 100) {
      density.value = 'Média';
    } else {
      density.value = 'Baixa';
    }

    // Agrupar por vizinhança
    const neighborhoods = groupByNeighbourhood({ listings: data.listings });
    // Converter objeto em array e ordenar por count
    const neighborhoodArray = Object.entries(neighborhoods)
      .map(([name, stats]) => ({
        name,
        count: stats.count,
        avgPrice: stats.avgPrice,
      }))
      .sort((a, b) => b.count - a.count);

    topAreas.value = neighborhoodArray.slice(0, 3);
  }

  onMounted(loadStats);
  watch(() => insideAirbnbStore.selectedCity, loadStats);
  watch(() => insideAirbnbStore.period, loadStats);
</script>

<style scoped></style>
