import { defineStore } from 'pinia';
import { ref, computed, watch } from 'vue';

export const useInsideAirbnbStore = defineStore('insideAirbnb', () => {
  // Estado - inicializar com valores do localStorage
  const selectedCity = ref(localStorage.getItem('selectedCity') || 'Porto');
  const period = ref(localStorage.getItem('selectedPeriod') || 'Q1 2024');
  const isLoading = ref(false);
  const selectedMenu = ref(1);

  // Getters
  const cityLabel = computed(() => {
    const cities = {
      Porto: 'Porto',
      Lisbon: 'Lisboa',
      Barcelona: 'Barcelona',
    };
    return cities[selectedCity.value] || selectedCity.value;
  });

  // Persistir no localStorage
  watch(selectedCity, (newCity) => {
    localStorage.setItem('selectedCity', newCity);
  });

  watch(period, (newPeriod) => {
    localStorage.setItem('selectedPeriod', newPeriod);
  });

  // Actions
  function setCity(city) {
    selectedCity.value = city;
  }

  function setPeriod(newPeriod) {
    period.value = newPeriod;
  }

  function setLoading(loading) {
    isLoading.value = loading;
  }

  function setSelectedMenu(menuId) {
    selectedMenu.value = menuId;
  }

  return {
    // Estado
    selectedCity,
    period,
    isLoading,
    selectedMenu,
    // Getters
    cityLabel,
    // Actions
    setCity,
    setPeriod,
    setLoading,
    setSelectedMenu,
  };
});
