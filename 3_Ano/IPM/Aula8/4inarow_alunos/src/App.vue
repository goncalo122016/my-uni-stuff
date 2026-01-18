<!-- Este documento apresenta uma solução em construção. É fornecido como 
     material de estudo. Recomenda-se que o revejam e melhorem conforme   
     forem adquirindo novos conhecimentos. -->

<script>
import OJogo from './components/OJogo.vue';
import Navbar from './components/Navbar.vue';


export default {
  components: {
    Navbar,
    OJogo      // equivalente a: 'OJogo': OJogo, permite também usar <o-jogo>
  },
  data() {
    return {
      jogador: false,
      vencedor: null
    }
  },
  methods: { 
    
  }
}
</script>

<template>
  <Navbar  />
  <OJogo 
    />
</template>
