<template>
  <div ref="rootRef" class="relative rounded-xl border border-gray-300 bg-white shadow-xl">
    <!-- Download button -->
    <button
      v-if="downloadable"
      @click="download"
      :class="[
        'z-60 absolute right-4 text-gray-600 hover:text-gray-400',
        props.icon_top ? 'top-4' : 'bottom-4',
      ]"
      title="Download"
    >
      <Download class="h-5 w-5" />
    </button>

    <!-- Card content -->
    <div class="p-8">
      <slot />
    </div>
  </div>
</template>

<script setup>
  import { ref } from 'vue';
  import html2canvas from 'html2canvas';
  import { Download } from 'lucide-vue-next';

  const props = defineProps({
    fileName: {
      type: String,
      default: 'export',
    },
    downloadable: {
      type: Boolean,
      default: true,
    },
    icon_top: {
      type: Boolean,
      default: true,
    },
  });

  const rootRef = ref(null);

  async function download() {
    console.log('AQUIIII...');
    if (!rootRef.value) return;

    const canvas = await html2canvas(rootRef.value, {
      backgroundColor: null,
      scale: 2,
      ignoreElements: (el) => el.tagName === 'BUTTON',
    });

    const link = document.createElement('a');
    link.download = `${props.fileName}.png`;
    link.href = canvas.toDataURL('image/png');
    link.click();
  }

  defineExpose({
    download,
  });
</script>
