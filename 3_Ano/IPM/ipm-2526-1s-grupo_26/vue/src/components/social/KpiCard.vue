<template>
  <DownloadableCard ref="downloadRef" :fileName="title">
    <div class="text-base text-gray-600">{{ title }}</div>
    <div :class="['mt-6 text-4xl', valueClass]">{{ value }}</div>
    <div class="mt-4 text-base text-gray-500">{{ subtitle }}</div>
    <div class="my-6 h-px bg-gray-100"></div>
    <div class="text-sm text-gray-400">{{ note }}</div>
  </DownloadableCard>
</template>

<script setup>
  import { ref } from 'vue';
  import DownloadableCard from '../DownloadableCard.vue';

  defineProps({
    title: String,
    value: String,
    subtitle: String,
    note: String,
    valueClass: {
      type: String,
      default: 'text-cyan-500',
    },
  });

  const downloadRef = ref(null);

  function download() {
    downloadRef.value?.download();
  }

  defineExpose({
    download,
  });
</script>
