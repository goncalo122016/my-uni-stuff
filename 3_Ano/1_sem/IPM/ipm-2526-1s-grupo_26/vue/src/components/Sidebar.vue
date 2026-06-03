<script setup>
  import { useRouter, useRoute } from 'vue-router';
  import {
    BarChart3,
    LineChart,
    PieChart,
    Users,
    MapPin,
    Download,
    ChevronRight,
  } from 'lucide-vue-next';

  import Logo from '../public/logo.png';

  const emit = defineEmits(['select-menu']);

  const menuItems = [
    { id: 1, label: 'Visão Geral', icon: BarChart3, route: '/' },
    { id: 2, label: 'Análise Temporal', icon: LineChart, route: '/time' },
    { id: 3, label: 'Painel Executivo', icon: PieChart, route: '/executive' },
    { id: 4, label: 'Gráficos Sociais', icon: Users, route: '/social' },
    { id: 5, label: 'Vista de Mapa', icon: MapPin, route: '/map' },
    { id: 6, label: 'Exportar Dados', icon: Download, route: '/export' },
  ];

  const router = useRouter();
  const route = useRoute();

  function onMenuClick(item) {
    emit('select-menu', item.id);
    router.push(item.route).catch(() => {});
  }
</script>

<template>
  <aside class="flex h-full w-64 flex-col border-r border-gray-300 bg-white text-gray-800">
    <div class="border-b border-gray-300 p-[25px]">
      <div class="flex items-center gap-3">
        <div
          class="flex h-14 w-14 flex-shrink-0 items-center justify-center rounded-2xl"
          style="background-color: var(--tw-color-primary)"
        >
          <img :src="Logo" alt="InsideAirbnb Logo" class="h-12 w-12" />
        </div>

        <div class="flex flex-col">
          <span class="text-2xl font-extrabold text-black">Inside</span>
          <span class="text-2xl font-extrabold text-primary">Airbnb</span>
        </div>
      </div>
    </div>

    <nav class="flex-1 space-y-2 overflow-y-auto p-4">
      <button
        v-for="item in menuItems"
        :key="item.id"
        @click="onMenuClick(item)"
        :class="[
          'flex w-full items-center gap-3 rounded-lg px-4 py-3 transition-colors duration-150',
          route.path === item.route ? 'bg-primary text-white' : 'hover:bg-gray-100',
        ]"
      >
        <component :is="item.icon" class="h-5 w-5" />
        <span class="flex-1 text-start text-sm font-medium">{{ item.label }}</span>
        <ChevronRight v-if="route.path === item.route" class="h-5 w-5" />
      </button>
    </nav>
  </aside>
</template>
