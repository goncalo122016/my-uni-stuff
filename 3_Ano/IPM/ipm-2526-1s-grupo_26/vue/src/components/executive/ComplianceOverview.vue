<template>
  <div class="rounded-xl p-6">
    <h3 class="text-lg font-bold text-gray-900">Visão geral da Conformidade</h3>
    <p class="mt-1 text-sm text-gray-500">Conformidade regulatória em diferentes categorias</p>

    <div class="mt-6 flex items-start gap-4">
      <!-- labels column (fixed width) -->
      <div class="w-44">
        <div v-for="c in compliance" :key="c.label + '-label'" class="py-2 text-sm text-gray-700">
          {{ c.label }}
        </div>
      </div>

      <!-- bars column inside a white rounded rectangle, closer to labels -->
      <div class="flex-1">
        <div class="rounded-lg border border-gray-200 bg-white p-3 shadow-sm">
          <div class="space-y-3">
            <div v-for="c in compliance" :key="c.label + '-bar'" class="py-1">
              <div class="h-8 w-full overflow-hidden rounded-full bg-gray-200">
                <div class="h-8 rounded-full bg-black" :style="{ width: getWidth(c) + '%' }"></div>
              </div>
            </div>
          </div>

          <!-- horizontal axis ticks (inside rectangle, below bars) -->
          <div class="mt-4 px-1">
            <div class="flex justify-between text-xs text-gray-500">
              <span v-for="t in ticks" :key="t">{{ t }}</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
  import { computed } from 'vue';

  const props = defineProps({
    compliance: { type: Array, required: true },
  });

  // Determine numeric value for each item. Accept either `value` (number) or `percent` fallback.
  const values = computed(() =>
    props.compliance.map((c) => (typeof c.value === 'number' ? c.value : c.percent || 0))
  );

  const ticks = computed(() => {
    const steps = 4;
    return Array.from({ length: steps + 1 }, (_, i) => Math.round((maxValue.value / steps) * i));
  });

  const maxValue = computed(() => Math.max(...values.value, 9000));

  function getWidth(c) {
    const v = typeof c.value === 'number' ? c.value : c.percent || 0;
    // guard
    if (!maxValue.value) return 0;
    return Math.min(100, (v / maxValue.value) * 100);
  }
</script>

<style scoped></style>
