<template>
  <LoadingPage v-if="isLoading" />
  <router-view />
  <Toaster />
</template>

<script setup>
  import LoadingPage from './components/loading/LoadingPage.vue';
  import { useLoading } from './components/loading/useLoading';
  import Toaster from './components/toaster/Toaster.vue';

  const { isLoading } = useLoading();
</script>
