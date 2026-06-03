<template>
  <div class="space-y-6">
    <TopCards :cards="topCards" />

    <AlertsList :alerts="alerts" />

    <div class="rounded-xl border border-gray-300 bg-white p-6 shadow-lg lg:col-span-2">
      <ZoneGrid :zones="zones" />
    </div>

    <div class="rounded-xl border border-gray-300 bg-white p-6 shadow-lg">
      <AnomaliesTable :anomalies="anomalies" />
    </div>
  </div>
</template>

<script setup>
  import { ref, watch, onMounted } from 'vue';
  import TopCards from './components/executive/TopCards.vue';
  import AlertsList from './components/executive/AlertsList.vue';
  import ZoneGrid from './components/executive/ZoneGrid.vue';
  import AnomaliesTable from './components/executive/AnomaliesTable.vue';
  import { loadCityData, calculateKPIs, groupByNeighbourhood } from '../data/backend.js';
  import { AlertTriangle, AlertCircle, Check, RefreshCw } from 'lucide-vue-next';
  import { useInsideAirbnbStore } from './stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();

  const activeTab = ref('critico');
  const topCards = ref([]);
  const alerts = ref([]);
  const zones = ref([]);
  const anomalies = ref([]);

  async function loadExecutiveData() {
    const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);

    if (!data || !data.listings || data.listings.length === 0) {
      console.warn('Nenhum dado para ExecutivePanel', insideAirbnbStore.selectedCity);
      return;
    }

    const listingsCount = data.listings.length || 0;
    const kpis = calculateKPIs(data);

    // Top Cards
    topCards.value = [
      {
        title: 'Alertas críticos',
        value: Math.floor(Math.max(1, listingsCount * 0.05)).toString(),
        subtitle: 'Requere ação imediata',
        border: 'border-red-400',
        icon: AlertTriangle,
      },
      {
        title: 'Avisos',
        value: Math.floor(Math.max(1, listingsCount * 0.1)).toString(),
        subtitle: 'Necessitam de revisão',
        border: 'border-yellow-400',
        icon: AlertCircle,
      },
      {
        title: 'Em revisão',
        value: Math.floor(Math.max(1, listingsCount * 0.1)).toString(),
        subtitle: 'Aguardando verificação',
        border: 'border-sky-300',
        icon: RefreshCw,
      },
    ];

    // Zones
    const neighbourhoods = groupByNeighbourhood({ listings: data.listings });
    zones.value = Object.keys(neighbourhoods).map((name) => {
      const n = neighbourhoods[name];
      return {
        name: name,
        listings: n.count,
        alerts: Math.floor(Math.max(0, n.count * 0.05)),
        status: n.count > 50 ? 'Alta' : n.count > 20 ? 'Média' : 'Baixa',
      };
    });

    // Alerts (gerados automaticamente)
    alerts.value = data.listings
      .filter((l) => {
        const price = parseFloat(l.price) || 0;
        const availability = parseFloat(l.availability_365) || 365;
        const reviews = parseFloat(l.number_of_reviews) || 0;
        return price > 200 || availability < 30 || reviews > 200;
      })
      .slice(0, 6)
      .map((l, idx) => ({
        id: idx + 1,
        title:
          parseFloat(l.availability_365 || 365) < 30
            ? 'Ocupação Elevada'
            : parseFloat(l.price) > 200
            ? 'Anomalia de preço'
            : 'Múltiplas avaliações',
        description: l.name,
        zone: l.neighbourhood,
        severity: Math.random() > 0.5 ? 'critical' : 'warning',
        date: new Date().toISOString().split('T')[0],
      }));

    // Anomalies (baseadas em dados reais)
    anomalies.value = data.listings
      .filter((l) => parseFloat(l.availability_365 || 365) < 30)
      .slice(0, 4)
      .map((l, idx) => ({
        id: idx + 1,
        property: l.name,
        problem:
          parseFloat(l.availability_365) < 30
            ? `${365 - parseFloat(l.availability_365)} dias ocupada`
            : 'Sem número de registo',
        zone: l.neighbourhood,
        severity: parseFloat(l.availability_365) < 20 ? 'Alta' : 'Média',
        date: new Date().toISOString().split('T')[0],
      }));
  }

  onMounted(loadExecutiveData);
  watch(() => insideAirbnbStore.selectedCity, loadExecutiveData);
  watch(() => insideAirbnbStore.period, loadExecutiveData);
</script>

<style scoped></style>
