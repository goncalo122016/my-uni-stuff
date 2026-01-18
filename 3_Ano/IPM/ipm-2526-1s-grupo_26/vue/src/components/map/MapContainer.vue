<template>
  <div
    class="overflow-hidden rounded-xl border border-gray-200 bg-white shadow-lg"
    style="height: 100%"
  >
    <div style="height: 100%; min-height: 360px">
      <l-map
        ref="leafletMap"
        :zoom="zoomLocal"
        :center="centerLocal"
        style="height: 100%; width: 100%; z-index: 0"
      >
        <l-tile-layer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />

        <!-- Marker para localização selecionada -->
        <l-marker v-if="marker" :lat-lng="marker" />

        <!-- Círculos para cada listagem -->
        <l-circle
          v-for="circle in listingCircles"
          :key="circle.id"
          :lat-lng="circle.center"
          :radius="circle.radius"
          :color="circle.color"
          :fill-color="circle.fillColor"
          :fill-opacity="circle.fillOpacity"
          :weight="circle.weight"
        />

        <l-control-zoom position="topright" />
        <l-control-attribution />
      </l-map>
    </div>
  </div>
</template>

<script setup>
  import { ref, onMounted, watch, nextTick, computed } from 'vue';
  import {
    LMap,
    LTileLayer,
    LMarker,
    LControlZoom,
    LControlAttribution,
    LCircle,
  } from '@vue-leaflet/vue-leaflet';

  const props = defineProps({
    center: { type: Array, default: () => [41.1579, -8.6291] },
    zoom: { type: Number, default: 13 },
    marker: { type: Array, default: null },
    listings: { type: Array, default: () => [] },
    filters: { type: Object, default: () => ({ propertyType: '', priceRange: '' }) },
  });

  const leafletMap = ref(null);
  const centerLocal = ref(props.center);
  const zoomLocal = ref(props.zoom);

  // Gerar círculos para as listagens
  const listingCircles = computed(() => {
    if (!props.listings || props.listings.length === 0) return [];

    // Filtrar listagens baseado em filtros
    let filtered = props.listings;

    if (props.filters.propertyType) {
      const typeMap = {
        entire: 'Entire home/apt',
        private: 'Private room',
        shared: 'Shared room',
      };
      const targetType = typeMap[props.filters.propertyType];
      filtered = filtered.filter((l) => l.room_type === targetType);
    }

    if (props.filters.priceRange) {
      const [minPrice, maxPrice] =
        props.filters.priceRange === '200+'
          ? [200, 999999]
          : props.filters.priceRange.split('-').map(Number);

      filtered = filtered.filter((l) => {
        const price = l.price || 0;
        return price >= minPrice && price < maxPrice;
      });
    }

    return filtered.slice(0, 100).map((listing, idx) => {
      // Usar coordenadas reais da listagem
      const lat = listing.latitude;
      const lng = listing.longitude;

      // Se não houver coordenadas, skip esta listagem
      if (!lat || !lng) return null;

      const price = listing.price || 50;
      const occupancy = listing.occupancy_rate || 0;

      // Cor baseada no preço
      let color = '#0d47a1'; // Azul escuro - preço baixo (0-50)
      if (price >= 50 && price < 200) {
        color = '#10b981'; // Verde - preço médio (50-200)
      } else if (price >= 200 && price < 500) {
        color = '#f59e0b'; // Laranja - preço alto (200-500)
      } else if (price >= 500) {
        color = '#0ea5e9'; // Azul claro - preço muito alto (500+)
      }

      return {
        id: listing.id || idx,
        center: [lat, lng],
        radius: Math.max(50, Math.min(500, price * 2)), // Raio baseado no preço
        color: color,
        fillColor: color,
        fillOpacity: 0.3 + (occupancy / 100) * 0.4, // Opacidade baseada na taxa de ocupação
        weight: 2,
      };
    });
  });

  onMounted(async () => {
    await nextTick();
    setTimeout(() => {
      try {
        const mapObj = leafletMap.value && leafletMap.value.mapObject;
        if (mapObj && typeof mapObj.invalidateSize === 'function') mapObj.invalidateSize(true);
      } catch (e) {}
    }, 200);
  });

  watch(
    () => props.center,
    (val) => {
      if (!val || val.length !== 2) return;
      centerLocal.value = [Number(val[0]), Number(val[1])];
      try {
        const mapObj = leafletMap.value && leafletMap.value.mapObject;
        if (mapObj && typeof mapObj.setView === 'function')
          mapObj.setView(centerLocal.value, mapObj.getZoom() || zoomLocal.value);
      } catch (e) {}
    }
  );

  watch(
    () => props.zoom,
    (v) => {
      zoomLocal.value = v;
    }
  );

  // expose map ref if parent wants it
  defineExpose({ leafletMap });
</script>

<style scoped></style>
