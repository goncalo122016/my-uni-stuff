<template>
  <div class="shadow-sm">
    <h3 class="mb-4 text-lg font-bold text-gray-900">Análise de zona</h3>
    <p class="mb-4 text-sm text-gray-500">Densidade de listagens e conformidade por zona</p>

    <div class="grid grid-cols-1 gap-4 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4">
      <div
        v-for="z in paginatedZones"
        :key="z.name"
        :class="['rounded-lg p-4 shadow-lg', borderClass(z.status), bgClass(z.status)]"
      >
        <div class="mb-3 flex items-center justify-between">
          <h4 class="font-semibold text-gray-800">{{ z.name }}</h4>
          <span :class="['rounded px-2 py-1 text-xs font-medium', badgeClass(z.status)]">{{
            z.status
          }}</span>
        </div>

        <div class="space-y-2 text-sm text-gray-600">
          <div class="flex items-center justify-between">
            <span class="text-sm">Listagens:</span>
            <span class="font-medium text-slate-800">{{ formatNumber(z.listings) }}</span>
          </div>
          <div class="flex items-center justify-between">
            <span class="text-sm">Alertas:</span>
            <span class="font-medium text-slate-800">{{ z.alerts }}</span>
          </div>
        </div>
      </div>
    </div>

    <!-- Pagination Controls -->
    <div
      v-if="totalPages > 1"
      class="mt-6 flex items-center justify-between border-t border-gray-200 pt-4"
    >
      <div class="text-sm text-gray-600">
        Mostrando <span class="font-semibold">{{ startIndex + 1 }}</span> a
        <span class="font-semibold">{{ Math.min(endIndex, zones.length) }}</span> de
        <span class="font-semibold">{{ zones.length }}</span> zonas
      </div>

      <div class="flex gap-2">
        <button
          @click="previousPage"
          :disabled="currentPage === 1"
          class="flex items-center gap-1 rounded-lg border border-gray-300 px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50 disabled:cursor-not-allowed disabled:opacity-50"
        >
          ← Anterior
        </button>

        <div class="flex items-center gap-1">
          <span v-for="page in visiblePages" :key="page">
            <button v-if="page === '...'" class="px-2 py-2 text-gray-500">...</button>
            <button
              v-else
              @click="goToPage(page)"
              :class="[
                'min-w-10 rounded-lg px-3 py-2 text-sm font-medium',
                page === currentPage
                  ? 'bg-blue-600 text-white'
                  : 'border border-gray-300 text-gray-700 hover:bg-gray-50',
              ]"
            >
              {{ page }}
            </button>
          </span>
        </div>

        <button
          @click="nextPage"
          :disabled="currentPage === totalPages"
          class="flex items-center gap-1 rounded-lg border border-gray-300 px-3 py-2 text-sm font-medium text-gray-700 hover:bg-gray-50 disabled:cursor-not-allowed disabled:opacity-50"
        >
          Próximo →
        </button>
      </div>
    </div>
  </div>
</template>

<script setup>
  import { ref, computed } from 'vue';

  const props = defineProps({
    zones: { type: Array, required: true },
  });

  const currentPage = ref(1);
  const itemsPerPage = 12;

  const totalPages = computed(() => Math.ceil(props.zones.length / itemsPerPage));

  const startIndex = computed(() => (currentPage.value - 1) * itemsPerPage);
  const endIndex = computed(() => currentPage.value * itemsPerPage);

  const paginatedZones = computed(() => {
    return props.zones.slice(startIndex.value, endIndex.value);
  });

  const visiblePages = computed(() => {
    const pages = [];
    const maxVisible = 5;
    let start = Math.max(1, currentPage.value - 2);
    let end = Math.min(totalPages.value, start + maxVisible - 1);

    if (end - start < maxVisible - 1) {
      start = Math.max(1, end - maxVisible + 1);
    }

    if (start > 1) {
      pages.push(1);
      if (start > 2) pages.push('...');
    }

    for (let i = start; i <= end; i++) {
      pages.push(i);
    }

    if (end < totalPages.value) {
      if (end < totalPages.value - 1) pages.push('...');
      pages.push(totalPages.value);
    }

    return pages;
  });

  function previousPage() {
    if (currentPage.value > 1) {
      currentPage.value--;
    }
  }

  function nextPage() {
    if (currentPage.value < totalPages.value) {
      currentPage.value++;
    }
  }

  function goToPage(page) {
    if (typeof page === 'number') {
      currentPage.value = page;
    }
  }

  function badgeClass(status) {
    if (!status) return 'bg-gray-100 text-gray-700';
    const s = status.toString().toLowerCase();
    if (s.includes('muito')) return 'bg-red-500 text-white';
    // show 'Alta' in red (requested) instead of amber
    if (s.includes('alta')) return 'bg-red-500 text-white';
    if (s.includes('média') || s.includes('media')) return 'bg-gray-100 text-gray-700';
    if (s.includes('baixa')) return 'bg-gray-100 text-gray-700';
    return 'bg-gray-100 text-gray-700';
  }

  function borderClass(status) {
    if (!status) return 'border border-gray-200';
    const s = status.toString().toLowerCase();
    if (s.includes('muito')) return 'border border-teal-300';
    if (s.includes('alta')) return 'border border-red-300';
    if (s.includes('média') || s.includes('media')) return 'border border-emerald-300';
    if (s.includes('baixa')) return 'border border-sky-300';
    return 'border border-gray-200';
  }

  function bgClass(status) {
    if (!status) return 'bg-white';
    const s = status.toString().toLowerCase();
    if (s.includes('muito')) return 'bg-teal-50';
    if (s.includes('alta')) return 'bg-red-50';
    if (s.includes('média') || s.includes('media')) return 'bg-white';
    if (s.includes('baixa')) return 'bg-sky-50';
    return 'bg-white';
  }

  function formatNumber(n) {
    if (n == null) return '—';
    return n.toLocaleString();
  }
</script>

<style scoped></style>
