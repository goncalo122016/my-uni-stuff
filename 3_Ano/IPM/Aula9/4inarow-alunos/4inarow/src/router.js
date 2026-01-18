import { createRouter, createWebHistory } from 'vue-router';
import GamePage from './pages/GamePage.vue';
import StatisticsPage from './pages/StatisticsPage.vue';
import SimulationPage from './pages/SimulationPage.vue';
import NotFoundPage from './pages/NotFoundPage.vue';
import GameResultPage from './pages/GameResultPage.vue';

const router = createRouter({
  history: createWebHistory(),
  routes: [
    { path: "/game", component: GamePage },
    { path: "/game/:id", component: GameResultPage },
    { path: "/statistics", component: StatisticsPage },
    { path: "/simulation", component: SimulationPage},
    { path: "/", redirect: "/game" },
    { path: "/:notFound(.*)", component: NotFoundPage }
  ]
});

export default router;