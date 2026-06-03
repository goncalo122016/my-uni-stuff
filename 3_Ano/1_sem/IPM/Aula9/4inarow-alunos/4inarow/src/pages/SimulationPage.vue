<script>
import { Game } from "../models/game";

export default {
  data() {
    return {
      game: new Game(),
      running: false,
      simulation: {}
    }
  },
  methods: {
    play(column) {
      this.game.play(column);
    },
    simulate() {
      console.log("Starting simulation...");
      this.running = true;
      const plays = this.simulation.plays;
      let index = 0;

      const intervalId = setInterval(() => {
        if (index < plays.length) {
          this.play(plays[index]);
          index++;
        } else {
          clearInterval(intervalId);
          this.running = false;
        }
      }, 500);
    },
    async getSimulation() {
      try {
        const response = await fetch('http://localhost:3000/simulation/1');

        if (!response.ok) {
          throw new Error('Something went wrong');
        }

        const data = await response.json();
        this.simulation = data;
        console.log("Simulation data:", data);
      } catch (error) {
        console.log(error);
      }
    }
  },
  created() {
    this.getSimulation();
  }
}
</script>

<template>
<div class="title">Simulate Last Game</div>
  <game-board :game="game"></game-board>
  <div class="button-container">
    <button-component @click="simulate">Simulate</button-component>
  </div>
</template>

<style scoped>
.title {
  font-size: 1.2rem;
  font-weight: 600;
  text-align: center;
  margin: 40px 0;
}

.button-container {
  display: flex;
  justify-content: center;
  margin-top: 40px;
}
</style>