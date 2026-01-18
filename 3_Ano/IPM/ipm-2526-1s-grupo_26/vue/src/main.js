import { createApp } from 'vue';
import { createPinia } from 'pinia';
import App from './App.vue';
import './style.css';

import 'leaflet/dist/leaflet.css';

import { Icon } from 'leaflet';
import markerUrl from 'leaflet/dist/images/marker-icon.png';
import markerRetinaUrl from 'leaflet/dist/images/marker-icon-2x.png';
import markerShadow from 'leaflet/dist/images/marker-shadow.png';

Icon.Default.mergeOptions({
  iconUrl: markerUrl,
  iconRetinaUrl: markerRetinaUrl,
  shadowUrl: markerShadow,
});

import router from './router';

const pinia = createPinia();

createApp(App).use(pinia).use(router).mount('#app');
