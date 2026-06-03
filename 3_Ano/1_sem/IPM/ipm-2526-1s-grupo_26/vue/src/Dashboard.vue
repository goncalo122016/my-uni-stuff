<template>
  <div class="flex h-screen bg-gray-50">
    <!-- Sidebar desktop -->
    <Sidebar
      class="hidden md:flex"
      :selected-menu="insideAirbnbStore.selectedMenu"
      @select-menu="insideAirbnbStore.setSelectedMenu($event)"
    />

    <!-- Sidebar mobile (overlay) -->
    <transition name="fade">
      <div
        v-if="showMobileSidebar"
        class="fixed inset-0 z-40 bg-black bg-opacity-40 md:hidden"
        @click="showMobileSidebar = false"
      ></div>
    </transition>

    <transition name="slide">
      <Sidebar
        v-if="showMobileSidebar"
        class="fixed left-0 top-0 z-50 h-full w-64 md:hidden"
        :selected-menu="insideAirbnbStore.selectedMenu"
        @select-menu="handleMobileSelect"
      />
    </transition>

    <main class="flex flex-1 flex-col overflow-auto">
      <Header
        :selected-menu="insideAirbnbStore.selectedMenu"
        :selected-city="insideAirbnbStore.selectedCity"
        :period="insideAirbnbStore.period"
        @toggle-sidebar="showMobileSidebar = true"
        @update:selectedCity="insideAirbnbStore.setCity($event)"
        @update:period="insideAirbnbStore.setPeriod($event)"
      />

      <div class="flex-1 overflow-auto">
        <div class="p-4 md:p-8">
          <router-view
            :selectedCity="insideAirbnbStore.selectedCity"
            :period="insideAirbnbStore.period"
            :key="insideAirbnbStore.selectedCity + '_' + insideAirbnbStore.period"
          />
        </div>
      </div>
    </main>
  </div>
</template>

<script setup>
  import Sidebar from './components/Sidebar.vue';
  import Header from './components/Header.vue';
  import { ref } from 'vue';
  import { useInsideAirbnbStore } from './stores/insideAirbnbStore';

  const insideAirbnbStore = useInsideAirbnbStore();
  const showMobileSidebar = ref(false);

  function handleMobileSelect(menuId) {
    insideAirbnbStore.setSelectedMenu(menuId);
    showMobileSidebar.value = false;
  }
</script>

<style>
  .fade-enter-active,
  .fade-leave-active {
    transition: opacity 0.25s;
  }
  .fade-enter-from,
  .fade-leave-to {
    opacity: 0;
  }

  .slide-enter-active,
  .slide-leave-active {
    transition: transform 0.25s;
  }
  .slide-enter-from {
    transform: translateX(-100%);
  }
  .slide-leave-to {
    transform: translateX(-100%);
  }
</style>
