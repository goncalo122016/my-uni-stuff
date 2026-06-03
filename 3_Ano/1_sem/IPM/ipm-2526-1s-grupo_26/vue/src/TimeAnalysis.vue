<template>
  <div class="space-y-6">
    <TimeKPIs />
    <TimeSeriesChart />
    <SeasonalPatterns />
  </div>
</template>

<script setup>
  import TimeKPIs from './components/time/TimeKPIs.vue';
  import TimeSeriesChart from './components/time/TimeSeriesChart.vue';
  import SeasonalPatterns from './components/time/SeasonalPatterns.vue';
</script>

<style scoped></style>
