<script setup>
  import { ref, watch } from 'vue';
  import { Layers } from 'lucide-vue-next';

  const emit = defineEmits(['update-filters']);

  const propertyType = ref('');
  const priceRange = ref('');

  function updateFilters() {
    emit('update-filters', {
      propertyType: propertyType.value,
      priceRange: priceRange.value,
    });
  }

  watch([propertyType, priceRange], () => {
    updateFilters();
  });
</script>

<template>
  <div class="space-y-3">
    <!-- Map layers header -->
    <h2 class="text-sm font-semibold text-gray-700">Camadas do mapa</h2>

    <!-- Map layer button -->
    <button
      class="flex w-full items-center gap-2 rounded-lg bg-[#0080a1] px-4 py-2.5 font-medium text-white shadow-md transition-colors hover:bg-[#006977]"
    >
      <Layers class="h-5 w-5" />
      Densidade de listagens
    </button>

    <!-- Filters section -->
    <div>
      <h3 class="mb-3 text-sm font-semibold text-gray-700">Filtros</h3>
      <div class="space-y-3">
        <!-- Property type filter -->
        <div>
          <label class="mb-2 block text-sm font-medium text-gray-700">Tipo de propriedade</label>
          <select
            v-model="propertyType"
            class="w-full rounded-lg border border-gray-300 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-[#0080a1]"
          >
            <option value="">Todos os tipos</option>
            <option value="entire">Casa inteira / apto</option>
            <option value="private">Quarto privado</option>
            <option value="shared">Quarto partilhado</option>
          </select>
        </div>

        <!-- Price range filter -->
        <div>
          <label class="mb-2 block text-sm font-medium text-gray-700">Intervalo de preço</label>
          <select
            v-model="priceRange"
            class="w-full rounded-lg border border-gray-300 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-[#0080a1]"
          >
            <option value="">Todos os preços</option>
            <option value="0-50">€0 - €50</option>
            <option value="50-100">€50 - €100</option>
            <option value="100-200">€100 - €200</option>
            <option value="200+">€200+</option>
          </select>
        </div>
      </div>
    </div>
  </div>
</template>

<style scoped></style>
