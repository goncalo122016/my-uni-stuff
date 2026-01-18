<script setup lang="ts">
  import { ref, watch, onMounted } from 'vue';
  import MapFilters from './components/map/MapFilters.vue';
  import MapContainer from './components/map/MapContainer.vue';
  import MapLegend from './components/map/MapLegend.vue';
  import MapStats from './components/map/MapStats.vue';
  import { loadCityData } from '../data/backend.js';
  import { useInsideAirbnbStore } from './stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();

  const props = defineProps({
    centerCoord: {
      type: Array,
      default: null,
    },
  });

  const cityCoordinates = {
    Porto: [41.1579, -8.6291],
    Lisbon: [38.7223, -9.1393],
    Barcelona: [41.3851, 2.1734],
  };

  const center = ref<[number, number]>(
    cityCoordinates[insideAirbnbStore.selectedCity] || [41.1579, -8.6291]
  );
  const zoom = ref<number>(13);
  const listings = ref([]);
  const filters = ref({ propertyType: '', priceRange: '' });

  // marker position (starts at default center)
  const markerLatLng = ref(center.value);

  // Carregar listagens quando a cidade muda
  async function loadListings() {
    insideAirbnbStore.setLoading(true);
    try {
      const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);
      if (data && data.listings) {
        listings.value = data.listings;
      }
    } finally {
      insideAirbnbStore.setLoading(false);
    }
  }

  watch(
    () => insideAirbnbStore.selectedCity,
    (newCity) => {
      const coords = cityCoordinates[newCity];
      if (coords) {
        center.value = [...coords];
        markerLatLng.value = [...coords];
      }
    }
  );

  watch(
    () => props.centerCoord,
    (val) => {
      if (!val || val.length !== 2) return;
      const lat = Number(val[0]);
      const lng = Number(val[1]);
      center.value = [lat, lng];
      markerLatLng.value = [lat, lng];
    },
    { immediate: true }
  );

  watch(() => insideAirbnbStore.selectedCity, loadListings);
  watch(() => insideAirbnbStore.period, loadListings);

  onMounted(loadListings);

  function handleFilterUpdate(newFilters) {
    filters.value = newFilters;
  }
</script>

<template>
  <div class="flex gap-6">
    <!-- Left sidebar: Filters (vertical layout) -->
    <div class="w-64 flex-shrink-0 overflow-y-auto border-r border-gray-200 bg-white p-4">
      <MapFilters @update-filters="handleFilterUpdate" />
    </div>

    <!-- Main content area: map, legend, stats -->
    <div class="flex flex-1 flex-col gap-6 overflow-y-auto">
      <!-- Map -->
      <div style="height: 480px; min-height: 360px; flex-shrink: 0">
        <MapContainer
          :center="center"
          :zoom="zoom"
          :marker="markerLatLng"
          :listings="listings"
          :filters="filters"
        />
      </div>

      <!-- Legend & Stats (2-column grid below map) -->
      <div class="grid grid-cols-2 gap-6 pr-4">
        <MapLegend />
        <MapStats />
      </div>
    </div>
  </div>
</template>
