<template>
  <header class="sticky top-0 z-10 border-b border-gray-300 bg-white shadow-sm">
    <div class="px-4 py-6 md:px-6 md:pb-9 md:pt-10">
      <div class="mb-4 flex items-center justify-between md:mb-0">
        <div class="flex items-center gap-3">
          <!-- MOBILE MENU BUTTON -->
          <button
            class="rounded-lg border border-gray-300 p-2 hover:bg-gray-100 md:hidden"
            @click="$emit('toggle-sidebar')"
          >
            <Menu />
          </button>
          <h1 class="text-2xl font-bold text-gray-900 md:text-3xl">{{ pageTitle }}</h1>
        </div>

        <div class="relative hidden items-center gap-3 md:flex">
          <!-- CITY BUTTON -->
          <div class="relative" ref="cityWrapper">
            <button
              @click.stop="toggleCityDropdown"
              class="flex cursor-pointer items-center gap-2 rounded-lg border border-gray-300 bg-white px-4 py-2 hover:bg-gray-50"
            >
              <MapPin class="h-4 w-4 text-gray-600" />
              <span class="text-sm font-medium text-gray-700">
                {{ selectedCity === 'Lisbon' ? 'Lisboa' : selectedCity }}
              </span>
              <ChevronDown class="h-4 w-4 text-gray-600" />
            </button>

            <!-- CITY DROPDOWN -->
            <div
              v-if="showCityDropdown"
              class="absolute left-0 z-50 mt-2 w-64 rounded-lg border border-gray-300 bg-white p-4 shadow-lg"
            >
              <h3 class="mb-2 text-xs font-semibold uppercase text-gray-500">Cidades</h3>

              <input
                v-model="citySearch"
                class="mb-3 w-full rounded-lg border border-gray-300 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
                placeholder="Search city..."
              />

              <div class="max-h-64 overflow-y-auto">
                <button
                  v-for="city in filteredCities"
                  :key="city.name"
                  @click="selectCity(city.name)"
                  class="flex w-full items-center justify-between rounded-lg px-3 py-2 hover:bg-gray-50"
                >
                  <div class="flex items-center gap-3">
                    <MapPin class="h-4 w-4 text-gray-400" />
                    <div>
                      <div class="text-start text-sm font-medium">
                        {{ city.name == 'Lisbon' ? 'Lisboa' : city.name }}
                      </div>
                      <div class="text-start text-xs text-gray-500">
                        {{ city.country == 'Spain' ? 'Espanha' : city.country }}
                      </div>
                    </div>
                  </div>
                  <Check v-if="selectedCity === city.name" class="h-4 w-4 text-gray-900" />
                </button>
              </div>
            </div>
          </div>

          <!-- PERIOD BUTTON -->
          <div class="relative" ref="periodWrapper">
            <button
              @click.stop="togglePeriodDropdown"
              class="flex cursor-pointer items-center gap-2 rounded-lg border border-gray-300 bg-white px-4 py-2 hover:bg-gray-50"
            >
              <Calendar class="h-4 w-4 text-gray-600" />
              <span class="text-sm font-medium text-gray-700">{{ selectedPeriod }}</span>
              <ChevronDown class="h-4 w-4 text-gray-600" />
            </button>

            <!-- PERIOD DROPDOWN -->
            <div
              v-if="showPeriodDropdown"
              class="absolute right-0 z-50 mt-2 w-72 rounded-lg border border-gray-300 bg-white p-4 shadow-lg"
            >
              <h3 class="mb-2 text-xs font-semibold uppercase text-gray-500">Quadrimestres</h3>

              <div class="space-y-1">
                <button
                  v-for="p in periods"
                  :key="p.id"
                  @click="selectPeriod(p.value)"
                  class="flex w-full items-center justify-between rounded-lg px-3 py-2 hover:bg-gray-50"
                >
                  <div class="flex items-center gap-3">
                    <Calendar class="h-4 w-4 text-gray-400" />
                    <div>
                      <div class="text-sm font-medium">{{ p.label }}</div>
                      <div class="text-xs text-gray-500">{{ p.year }}</div>
                    </div>
                  </div>
                  <Check v-if="selectedPeriod === p.value" class="h-4 w-4 text-gray-900" />
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div class="flex items-center gap-3 md:hidden">
        <!-- CITY (MOBILE) -->
        <div class="relative flex-1" ref="cityWrapper">
          <button
            @click.stop="toggleCityDropdown"
            class="flex w-full items-center justify-center gap-2 rounded-lg border border-gray-300 bg-white px-3 py-2 hover:bg-gray-50"
          >
            <MapPin class="h-4 w-4 text-gray-600" />
            <span class="text-sm font-medium">{{ selectedCity }}</span>
            <ChevronDown class="h-4 w-4 text-gray-600" />
          </button>

          <!-- MOBILE CITY DROPDOWN -->
          <div
            v-if="showCityDropdown"
            class="absolute left-0 right-0 z-50 mt-2 w-full rounded-lg border border-gray-300 bg-white p-4 shadow-lg"
          >
            <h3 class="mb-2 text-xs font-semibold uppercase text-gray-500">Cidades</h3>

            <input
              v-model="citySearch"
              class="mb-3 w-full rounded-lg border border-gray-300 px-3 py-2 text-sm"
              placeholder="Search city..."
            />

            <div class="max-h-64 overflow-y-auto">
              <button
                v-for="city in filteredCities"
                :key="city.name"
                @click="selectCity(city.name)"
                class="flex w-full items-center justify-between rounded-lg px-3 py-2 hover:bg-gray-50"
              >
                <div class="flex items-center gap-3">
                  <MapPin class="h-4 w-4 text-gray-400" />
                  <div>{{ city.name == 'Lisbon' ? 'Lisboa' : city.name }}</div>
                </div>
                <Check v-if="selectedCity === city.name" class="h-4 w-4 text-gray-900" />
              </button>
            </div>
          </div>
        </div>

        <!-- PERIOD (MOBILE) -->
        <div class="relative flex-1" ref="periodWrapper">
          <button
            @click.stop="togglePeriodDropdown"
            class="flex w-full items-center justify-center gap-2 rounded-lg border border-gray-300 bg-white px-3 py-2 hover:bg-gray-50"
          >
            <Calendar class="h-4 w-4 text-gray-600" />
            <span class="text-sm font-medium">{{ selectedPeriod }}</span>
            <ChevronDown class="h-4 w-4 text-gray-600" />
          </button>

          <!-- MOBILE PERIOD DROPDOWN -->
          <div
            v-if="showPeriodDropdown"
            class="absolute left-0 right-0 z-50 mt-2 w-full rounded-lg border border-gray-300 bg-white p-4 shadow-lg"
          >
            <h3 class="mb-2 text-xs font-semibold uppercase text-gray-500">Quadrimestres</h3>

            <div class="space-y-1">
              <button
                v-for="p in periods"
                :key="p.id"
                @click="selectPeriod(p.value)"
                class="flex w-full items-center justify-between rounded-lg px-3 py-2 hover:bg-gray-50"
              >
                <div class="flex items-center gap-3">
                  <Calendar class="h-4 w-4 text-gray-400" />
                  <div class="text-sm">{{ p.label }}</div>
                </div>
                <Check v-if="selectedPeriod === p.value" class="h-4 w-4 text-gray-900" />
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  </header>
</template>

<script setup>
  import { ref, computed } from 'vue';
  import citiesData from '../cities.json';
  import { Calendar, MapPin, ChevronDown, Check, Menu } from 'lucide-vue-next';

  const props = defineProps({
    selectedMenu: Number,
    selectedCity: String,
    period: String,
  });

  const selectedCity = ref(localStorage.getItem('selectedCity') || 'Porto');
  const selectedPeriod = ref(localStorage.getItem('selectedPeriod') || 'Q1 2024');

  const showCityDropdown = ref(false);
  const showPeriodDropdown = ref(false);

  const citySearch = ref('');
  const cities = ref(citiesData);

  const periods = [
    { id: 1, label: '1º Quadrimestre', year: 'Ano: 2024', value: 'Q1 2024' },
    { id: 2, label: '2º Quadrimestre', year: 'Ano: 2024', value: 'Q2 2024' },
    { id: 3, label: '3º Quadrimestre', year: 'Ano: 2024', value: 'Q3 2024' },
    { id: 4, label: '4º Quadrimestre', year: 'Ano: 2024', value: 'Q4 2024' },
  ];

  const filteredCities = computed(() =>
    !citySearch.value
      ? cities.value
      : cities.value.filter((c) => c.name.toLowerCase().includes(citySearch.value.toLowerCase()))
  );

  function toggleCityDropdown() {
    showCityDropdown.value = !showCityDropdown.value;
    showPeriodDropdown.value = false;
  }

  function togglePeriodDropdown() {
    showPeriodDropdown.value = !showPeriodDropdown.value;
    showCityDropdown.value = false;
  }

  function selectCity(cityName) {
    selectedCity.value = cityName;
    localStorage.setItem('selectedCity', cityName);
    showCityDropdown.value = false;
    citySearch.value = '';
    emit('update:selectedCity', cityName);
  }

  function selectPeriod(period) {
    selectedPeriod.value = period;
    localStorage.setItem('selectedPeriod', period);
    showPeriodDropdown.value = false;
    emit('update:period', period);
  }

  const emit = defineEmits(['update:selectedCity', 'update:period', 'toggle-sidebar']);

  const pageTitle = computed(() => {
    const menu = Number(props.selectedMenu);
    return (
      {
        2: 'Análise Temporal',
        3: 'Painel Executivo',
        4: 'Gráficos Sociais',
        5: 'Vista de Mapa',
        6: 'Exportar Dados',
      }[menu] || 'Visão Geral'
    );
  });
</script>
