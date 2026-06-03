<template>
  <div class="mb-6 rounded-xl border border-gray-300 bg-white p-6 shadow-lg">
    <div class="flex items-start justify-between">
      <div>
        <h2 class="text-lg font-semibold text-gray-900">{{ title }}</h2>
        <p class="mt-1 text-sm text-gray-500">{{ description }}</p>
      </div>
    </div>

    <div class="mt-4 flex items-center gap-3">
      <button
        @click="emit('download-all')"
        class="inline-flex items-center gap-2 rounded bg-primary px-4 py-2 text-sm text-white shadow-lg transition-opacity hover:opacity-90"
      >
        <slot name="primary-icon">
          <FolderDown class="h-4 w-4" />
        </slot>
        {{ primaryAction }}
      </button>

      <button
        @click="emit('share-collection')"
        class="inline-flex items-center gap-2 rounded border border-gray-300 bg-white px-3 py-2 text-sm text-gray-700 transition-colors hover:bg-gray-50"
      >
        <slot name="secondary-icon">
          <Share2 class="h-4 w-4 text-gray-600" />
        </slot>
        {{ secondaryAction }}
      </button>

      <button
        @click="emit('copy-link')"
        class="inline-flex items-center gap-2 rounded border border-gray-300 bg-white px-3 py-2 text-sm text-gray-700 transition-colors hover:bg-gray-50"
      >
        <Link class="h-4 w-4 text-gray-600" />
        {{ tertiaryAction }}
      </button>
    </div>
  </div>
</template>

<script setup>
  import { FolderDown, Share2, Link } from 'lucide-vue-next';

  const props = defineProps({
    title: { type: String, default: 'Gráficos Sociais' },
    description: {
      type: String,
      default: 'Crie gráficos simples e impactantes otimizados para Instagram, Twitter e Facebook',
    },
    primaryAction: { type: String, default: 'Download All' },
    secondaryAction: { type: String, default: 'Partilhar coleção' },
    tertiaryAction: { type: String, default: 'Copiar link' },
  });

  const emit = defineEmits(['download-all', 'share-collection', 'copy-link']);
</script>

<style scoped></style>
