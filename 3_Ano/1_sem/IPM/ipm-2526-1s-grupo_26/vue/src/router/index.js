import { createRouter, createWebHistory } from 'vue-router';
import { useLoading } from '../components/loading/useLoading';
// layout
import DashboardLayout from '../Dashboard.vue';

// pages
import Overview from '../Overview.vue';
import TimeAnalysis from '../TimeAnalysis.vue';
import ExecutivePanel from '../ExecutivePanel.vue';
import SocialGraphs from '../SocialGraphs.vue';
import MapView from '../MapView.vue';
import ExportData from '../ExportData.vue';
import NotFound from '../404NotFound.vue';

const routes = [
  {
    path: '/',
    component: DashboardLayout,
    children: [
      { path: '', name: 'Overview', component: Overview },
      { path: 'time', name: 'TimeAnalysis', component: TimeAnalysis },
      { path: 'executive', name: 'Executive', component: ExecutivePanel },
      { path: 'social', name: 'Social', component: SocialGraphs },
      { path: 'map', name: 'MapView', component: MapView },
      { path: 'export', name: 'Export', component: ExportData },
    ],
  },

  {
    path: '/:pathMatch(.*)*',
    name: '404NotFound',
    component: NotFound,
  },
];

const router = createRouter({
  history: createWebHistory(),
  routes,
});

router.beforeEach(() => {
  const { startLoading } = useLoading();
  startLoading();
});

router.afterEach(() => {
  const { stopLoading } = useLoading();
  setTimeout(stopLoading, 400); // pequeno delay p/ suavidade
});

export default router;
