<script>
import { Game } from "../models/game";
import { PlayResult } from "../models/playResult";
import { GameResult } from "../models/gameResult";

export default {
  data() {
    return {
      game: new Game(),
      startPlayer: undefined,
      plays: [],
      statistics: {}
    }
  },
  methods: {
    async getStatistics() {
      try {
        const response = await fetch('http://localhost:3000/statistics/1');

        if (!response.ok) {
          throw new Error('Something went wrong');
        }

        this.statistics = await response.json();
      } catch (error) {
        console.log(error);
      }
    },
    async updateSimulation() {
      const data = {
        id: 1,
        startPlayer: this.startPlayer,
        plays: this.plays
      };
      console.log("Last game data:", data);

      try{
        const response = await fetch('http://localhost:3000/simulation/1', {
          method: 'PATCH',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(data)
        });

        if (!response.ok) {
          throw new Error('Something went wrong');
        }
      } catch (error) {
        console.log(error);
      }
    },
    async updateStatistics() {
      const winner = this.game.winner;
      if (winner === GameResult.RED) {
        this.statistics.red += 1;
      } else if (winner === GameResult.YELLOW) {
        this.statistics.yellow += 1;
      } else if (winner === GameResult.DRAW) {
        this.statistics.draw += 1;
      }
      try {
        const response = await fetch('http://localhost:3000/statistics/1', {
          method: 'PATCH',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(this.statistics)
        });

        if (!response.ok) {
          throw new Error('Something went wrong');
        }
      } catch (error) {
        console.log(error);
      }      
    },
    randomString(length) {
      let result = '';
      const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
      const charactersLength = characters.length;
      for (let i = 0; i < length; i++) {
        result += characters.charAt(Math.floor(Math.random() * charactersLength));
      }
      return result;
    },
    async saveGame() {
      const gameData = {
        id: this.randomString(4),
        game: {
          board: this.game.board,
          player: this.game.player,
          winner: this.game.winner,
          isOver: this.game.isOver
        },
        date: new Date().toISOString()
      };

      try {
        const response = await fetch('http://localhost:3000/games', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json'
          },
          body: JSON.stringify(gameData)
        });

        if (!response.ok) {
          throw new Error('Something went wrong');
        }
      } catch (error) {
        console.log(error);
      }
    },
    play(column) {
      const res = this.game.play(column);
      this.plays.push(column);

      if (res === PlayResult.ERROR_FULL_COLUMN) {
        alert('Column is full!');
      } else if (res === PlayResult.ERROR_GAME_OVER) {
        alert('Game over. Click "New Game" to play another game.')
      } else if (res === PlayResult.GAME_OVER) {
        this.updateStatistics();
        this.saveGame();
      }
    },
    reset() {
      this.game.reset();
      this.startPlayer = this.game.player;
      this.updateSimulation();
      this.plays = [];
    }
  },
  computed: {
    gameStatus() {
      const winner = this.game.winner;

      if (winner === GameResult.YELLOW) {
        return 'Winner: Yellow';
      }

      if (winner === GameResult.RED) {
        return 'Winner: Red';
      }

      if (winner === GameResult.DRAW) {
        return 'Draw';
      }

      return this.game.player
        ? 'Player: Red'
        : 'Player: Yellow'
    }
  },
  created() {
    this.getStatistics();
  }
}
</script>

<template>
  <div class="status">{{ gameStatus }}</div>
  <game-board :game="game" @play="play" />
  <div class="button-container">
    <button-component @click="reset">New Game</button-component>
  </div>
</template>

<style scoped>
.status {
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