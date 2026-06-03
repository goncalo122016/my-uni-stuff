<script>
import { Jogo } from "../models/jogo";
import { ResultadoJogada } from "../models/resultadoJogada";
import Button from "./Button.vue";

export default {
  components: {
    Button
  },
  data() {
    return {
      jogo : new Jogo(),
      numColunas: Jogo.NUMERO_COLUNAS,
      numLinhas: Jogo.NUMERO_LINHAS,
    }
  },
  methods: {
    jogar(coluna) {
      return this.jogo.jogar(coluna);
    },
    reset() {
      this.jogo.reset();
    },
    aplicarVermelho(coluna, linha) {
      return this.jogo.getCelula(coluna, linha) === true;
    },
    aplicarAmarelo(coluna, linha) {
      return this.jogo.getCelula(coluna, linha) === false;
    }
  }
}
</script>

<template>
  <div class="tabuleiro">
    <div v-for="c in numColunas" :key="c" class="coluna" @click="jogar(c - 1)">
      <div v-for="l in numLinhas" :key="l"
        class="celula"
        :class="{
          red: aplicarVermelho(c - 1, l - 1),
          yellow: aplicarAmarelo(c - 1, l - 1)
        }"
        >
      </div>
    </div>
  </div>
  <div class="button-container">
    <Button @click="reset()">Reset</Button>
  </div>
</template>

<style scoped>
.tabuleiro {
  display: flex;
  width: 40%;
  margin: 20px auto auto;
  background-color: #4357A0;
  padding: 10px;
  border-radius: 10px;
}

.coluna {
  display: flex;
  flex-direction: column;
  flex-grow: 1;
}

.celula {
  border-radius: 100%;
  background-color: #001773;
  max-height: 100%;
  max-width: 100%;
  aspect-ratio: 1;
  margin: 4px;
}

.yellow {
  background-color: var(--color-yellow);
}

.red {
  background-color: var(--color-red);
}

.button-container {
  display: flex;
  justify-content: center;
  margin-top: 40px;
}
</style>