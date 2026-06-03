<template>
  <div class="grid grid-cols-1 gap-6 shadow-sm md:grid-cols-3">
    <div
      v-for="(card, idx) in cards"
      :key="card.title"
      :class="['rounded-lg border bg-white p-4 shadow-lg', getBorder(card, idx)]"
      :style="isLast(idx) ? { borderColor: lastHex } : null"
    >
      <div class="flex items-start justify-between">
        <div>
          <div class="text-xs font-semibold text-gray-500">
            {{ card.title }}
          </div>
          <div
            :class="['mt-2 font-bold', getTextColor(card, idx), 'text-3xl']"
            :style="isLast(idx) ? { color: lastHex } : null"
          >
            {{ card.value }}
          </div>
          <div class="mt-1 text-sm text-gray-500">{{ card.subtitle }}</div>
        </div>

        <div
          :class="['flex h-8 w-8 items-center justify-center rounded-full', getIconBg(card, idx)]"
          :style="isLast(idx) ? { color: lastHex } : null"
        >
          <component :is="card.icon" :class="['h-3 w-3', getTextColor(card, idx)]" />
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
  const props = defineProps({
    cards: { type: Array, required: true },
  });

  const colorMap = {
    red: {
      border: 'border-red-600',
      text: 'text-red-600',
      bg: 'bg-red-50',
    },
    yellow: {
      border: 'border-yellow-300',
      text: 'text-yellow-300',
      bg: 'bg-yellow-50',
    },
    emerald: {
      border: 'border-emerald-300',
      text: 'text-emerald-600',
      bg: 'bg-emerald-50',
    },
    sky: { border: 'border-sky-600', text: 'text-sky-600', bg: 'bg-sky-50' },
    gray: { border: 'border-gray-50', text: 'text-gray-700', bg: 'bg-gray-50' },
  };

  const lastHex = '#636363';

  function extractColorKey(borderClass) {
    if (!borderClass) return 'gray';
    const parts = borderClass.split('-');
    if (parts.length >= 2) return parts[1];
    return 'gray';
  }

  function getBorder(card, idx) {
    // if third card (idx===2) force blue (sky)
    if (idx === 2) return colorMap.sky.border;
    const key = extractColorKey(card.border);
    return colorMap[key] ? colorMap[key].border : colorMap.gray.border;
  }

  function getTextColor(card, idx) {
    if (idx === 2) return colorMap.sky.text;
    const key = extractColorKey(card.border);
    return colorMap[key] ? colorMap[key].text : colorMap.gray.text;
  }

  function getIconBg(card, idx) {
    if (idx === 2) return colorMap.sky.bg;
    const key = extractColorKey(card.border);
    return colorMap[key] ? colorMap[key].bg : colorMap.gray.bg;
  }

  function isLast(idx) {
    return idx === props.cards.length - 1;
  }
</script>

<style scoped></style>
