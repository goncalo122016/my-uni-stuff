<template>
  <div class="mb-8 grid grid-cols-1 gap-6 md:grid-cols-2 lg:grid-cols-4">
    <DownloadableCard
      v-for="kpi in kpis"
      :key="kpi.title"
      :fileName="kpi.title.replace(/\s+/g, '_')"
      :icon_top="false"
    >
      <div class="mb-4 flex items-start justify-between">
        <div>
          <span class="text-xs font-semibold uppercase tracking-wide text-gray-500">{{
            kpi.title
          }}</span>
        </div>
        <div :class="['flex h-12 w-12 items-center justify-center rounded-lg', kpi.iconBg]">
          <component :is="kpi.icon" :class="['h-6 w-6', kpi.iconColor]" />
        </div>
      </div>
      <div class="mb-3">
        <div class="text-4xl font-bold text-gray-900">
          {{ kpi.value }}
        </div>
      </div>
      <div class="flex items-center gap-2">
        <component
          :is="kpi.trend === 'up' ? TrendingUp : TrendingDown"
          :class="['h-4 w-4', kpi.trend === 'up' ? 'text-emerald-600' : 'text-red-600']"
        />
        <span
          :class="[
            'text-sm font-semibold',
            kpi.trend === 'up' ? 'text-emerald-600' : 'text-red-600',
          ]"
        >
          {{ kpi.change }}
        </span>
        <span class="text-sm text-gray-500">{{ kpi.description }}</span>
      </div>
    </DownloadableCard>
  </div>
</template>

<script setup>
  import { ref, watch, onMounted } from 'vue';
  import {
    TrendingUp,
    TrendingDown,
    Building2,
    DollarSign,
    Calendar,
    Users,
  } from 'lucide-vue-next';
  import { loadCityData, calculateKPIs } from '../../../data/backend.js';
  import DownloadableCard from '../DownloadableCard.vue';
  import { useInsideAirbnbStore } from '../../stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();

  const kpis = ref([]);

  async function loadKPIs() {
    const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);
    if (!data || !data.listings || data.listings.length === 0) {
      console.warn('Nenhum dado carregado para', insideAirbnbStore.selectedCity);
      return;
    }

    const kpiData = calculateKPIs(data);
    const { totalListings, avgPrice, avgReviews, occupancyRate, activeListings } = kpiData;

    // Contar anfitriões únicos
    const uniqueHosts = new Set(data.listings.map((l) => l.host_id)).size;

    kpis.value = [
      {
        title: 'Total de Listagens',
        value: (totalListings || 0).toLocaleString(),
        change: '18.2%',
        description: 'desde o último trimestre',
        trend: 'up',
        icon: Building2,
        iconBg: 'bg-blue-50',
        iconColor: 'text-blue-600',
      },
      {
        title: 'Preço Médio',
        value: `€${avgPrice || 0}`,
        change: '3.5%',
        description: 'por noite',
        trend: 'up',
        icon: DollarSign,
        iconBg: 'bg-green-50',
        iconColor: 'text-green-600',
      },
      {
        title: 'Taxa de Ocupação',
        value: `${occupancyRate || 0}%`,
        change: '2.1%',
        description: 'média anual',
        trend: (occupancyRate || 0) > 70 ? 'up' : 'down',
        icon: Calendar,
        iconBg: 'bg-purple-50',
        iconColor: 'text-purple-600',
      },
      {
        title: 'Anfitriões Ativos',
        value: (uniqueHosts || 0).toLocaleString(),
        change: '12.3%',
        description: 'anfitriões únicos',
        trend: 'up',
        icon: Users,
        iconBg: 'bg-blue-50',
        iconColor: 'text-blue-600',
      },
    ];
  }

  onMounted(loadKPIs);

  watch(() => insideAirbnbStore.selectedCity, loadKPIs);
  watch(() => insideAirbnbStore.period, loadKPIs);
</script>

<style scoped></style>
