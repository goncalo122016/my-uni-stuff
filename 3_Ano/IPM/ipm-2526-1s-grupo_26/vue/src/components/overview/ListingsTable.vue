<template>
  <DownloadableCard fileName="Listagens-Recentes">
    <div class="overflow-hidden bg-white">
      <div class="border-b border-gray-300 p-6">
        <h3 class="text-lg font-bold text-gray-900">Listagens Recentes</h3>
        <p class="mt-1 text-sm text-gray-500">Últimas propriedades adicionadas à plataforma</p>
      </div>
      <div class="overflow-x-auto">
        <table class="w-full">
          <thead class="border-b border-gray-300 bg-gray-50">
            <tr>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Nome
              </th>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Tipo
              </th>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Zona
              </th>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Preço/Noite
              </th>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Avaliações
              </th>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Ocupação
              </th>
              <th
                class="px-6 py-4 text-left text-xs font-bold uppercase tracking-wide text-gray-700"
              >
                Anfitrião
              </th>
              <th class="px-6 py-4"></th>
            </tr>
          </thead>
          <tbody class="divide-y divide-gray-200">
            <tr
              v-for="listing in recentListings"
              :key="listing.id"
              class="transition-colors hover:bg-gray-50"
            >
              <td class="px-6 py-4 text-sm font-medium text-gray-900">{{ listing.name }}</td>
              <td class="px-6 py-4 text-sm text-gray-600">{{ listing.type }}</td>
              <td class="px-6 py-4 text-sm text-gray-600">{{ listing.zone }}</td>
              <td class="px-6 py-4 text-sm font-semibold text-gray-900">{{ listing.price }}</td>
              <td class="px-6 py-4 text-sm text-gray-600">{{ listing.reviews }}</td>
              <td class="px-6 py-4">
                <div class="flex items-center gap-2">
                  <div class="h-2 w-16 overflow-hidden rounded-full bg-gray-200">
                    <div
                      class="h-full rounded-full bg-gradient-to-r from-emerald-400 to-emerald-600"
                      :style="{ width: listing.occupancy + '%' }"
                    ></div>
                  </div>
                  <span
                    :class="[
                      'whitespace-nowrap text-sm font-bold',
                      listing.occupancy >= 80
                        ? 'text-emerald-600'
                        : listing.occupancy >= 60
                        ? 'text-blue-600'
                        : 'text-gray-600',
                    ]"
                    >{{ listing.occupancy }}%</span
                  >
                </div>
              </td>
              <td class="px-6 py-4 text-sm text-gray-600">{{ listing.host }}</td>
              <td class="px-6 py-4"></td>
            </tr>
          </tbody>
        </table>
      </div>
    </div>
  </DownloadableCard>
</template>

<script setup>
  import { ref, watch, onMounted } from 'vue';
  import { loadCityData, getTopListings } from '../../../data/backend.js';
  import DownloadableCard from '../DownloadableCard.vue';
  import { useInsideAirbnbStore } from '../../stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();

  const recentListings = ref([]);

  async function loadListings() {
    const data = await loadCityData(insideAirbnbStore.selectedCity, insideAirbnbStore.period);

    if (!data || !data.listings) {
      recentListings.value = [];
      return;
    }

    const topListings = getTopListings(data, 5);
    recentListings.value = topListings.map((listing) => ({
      id: listing.id,
      name: listing.name,
      type: listing.roomType,
      zone: listing.neighbourhood,
      price: listing.price,
      reviews: listing.reviews,
      occupancy: listing.occupancy,
      host: listing.host,
    }));
  }

  onMounted(loadListings);
  watch(() => insideAirbnbStore.selectedCity, loadListings);
  watch(() => insideAirbnbStore.period, loadListings);
</script>

<style scoped></style>
