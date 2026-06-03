<script>
import { Game } from "../models/game.js";

export default {
  props: ['id'],
  data() {
    return {
      game: new Game()
    };
  },
  methods: {
    async getGame() {
      const gameId = this.$route.params.id;
      try {
        const response = await fetch(`http://localhost:3000/games/${gameId}`);

        if (!response.ok) {
          throw new Error('Something went wrong');
        }

        const data = await response.json();
        this.game.board = data.game.board;
        this.game.winner = data.game.winner;
      } catch (error) {
        console.log(error);
      }
    }
  },
  created() {
    this.getGame();
  }
}
</script>

<template>
  <game-board :game="game"></game-board>
</template>