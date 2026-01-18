/* Este documento apresenta uma solução em construção. É fornecido como 
   material de estudo. Recomenda-se que o revejam e melhorem conforme   
   forem adquirindo novos conhecimentos. */

/* 
Importa o objeto ResultadoJogada do ficheiro resultadoJogada.js e o objeto ResultadoJogo do ficheiro resultadoJogo.js.
*/
import { ResultadoJogada } from "./resultadoJogada";
import { ResultadoJogo } from "./resultadoJogo";


/* 
Este ficheiro exporta uma classe Jogo que representa o jogo 4 em linha.
*/
export class Jogo {
  static NUMERO_COLUNAS = 7;
  static NUMERO_LINHAS = 6;

  tabuleiro = [];         // Array de arrays com o estado do tabuleiro.
  jogadorAJogar = false;  // Indica o jogador que tem a vez de jogar. false - jogador amarelo, true - jogador vermelho.
  vencedor = null;        // Indica o vencedor do jogo (cf. ResultadoJogo). null - jogo em curso.
  terminado = false;      // Indica se o jogo terminou.

  /*
  O construtor da classe Jogo inicializa o tabuleiro, o jogador a jogar e o vencedor.
  */
  constructor() {
    this.reset();
  }

  /*
  A função jogar(coluna) recebe um inteiro coluna e devolve um dos seguintes valores:
  - ResultadoJogada.ERRO_JOGOTERMINADO - O jogo já terminou.
  - ResultadoJogada.ERRO_COLUNAINVALIDA - A coluna não é válida.
  - ResultadoJogada.ERRO_COLUNACHEIA - A coluna está cheia.
  - ResultadoJogada.TERMINOU - A jogada terminou o jogo.
  - ResultadoJogada.SUCESSO - A jogada foi realizada com sucesso e o jogo continua.
  */
  jogar(coluna) {
    if (this.terminado) {
      return ResultadoJogada.ERRO_JOGOTERMINADO;
    }

    if (coluna < 0 || coluna >= Jogo.NUMERO_COLUNAS) {
      return ResultadoJogada.ERRO_COLUNAINVALIDA;
    }

    if (this.tabuleiro[coluna].length == Jogo.NUMERO_LINHAS) {
      return ResultadoJogada.ERRO_COLUNACHEIA;
    }

    this.tabuleiro[coluna].push(this.jogadorAJogar);  // Adiciona a jogada ao tabuleiro.
    this.jogadorAJogar = !this.jogadorAJogar;       // Alterna o jogador a jogar. 
    this.validarSeTerminou();

    return this.terminado ? ResultadoJogada.TERMINOU : ResultadoJogada.SUCESSO;
  }

  /*
  A função validarSeTerminou() verifica se o jogo terminou e quem é o vencedor.
  */
  validarSeTerminou() {
    for (let index = 0; index < this.tabuleiro.length; index++) {
      const coluna = this.tabuleiro[index];
      for (let index2 = 0; index2 < coluna.length; index2++) {
        // Construir a string com as jogadas da linha que começa na posição (index,index2) (nota: não existir jogada numa posição o valor será 'null'/'undefined')
        let linha = this.getCoordenadasLinha(index, index2)
                                 .map((x) => this.getCelulaInterna(x[0],x[1]))
                                 .join("");
        // Se a string contiver 4 'false' seguidos, o jogador amarelo venceu. Se a string contiver 4 'true' seguidos, o jogador vermelho venceu. Caso contrário, não há vencedor.
        this.vencedor = linha.includes("falsefalsefalsefalse") ? ResultadoJogo.AMARELO :
                        (linha.includes("truetruetruetrue") ? ResultadoJogo.VERMELHO : null);
        // Se houver um vencedor, o jogo terminou.
        if (this.vencedor !== null) {
          this.terminado = true;
          return;
        }
        // Construir a string com as jogadas da coluna que começa na posição (index,index2)
        let coluna = this.getCoordenadasColuna(index, index2)
                                 .map((x) => this.getCelulaInterna(x[0],x[1]))
                                 .join("");
        // Ver se alguém venceu na coluna.
        this.vencedor = coluna.includes("falsefalsefalsefalse") ? ResultadoJogo.AMARELO :
                        (coluna.includes("truetruetruetrue") ? ResultadoJogo.VERMELHO : null);
        // Se houver um vencedor, o jogo terminou.
        if (this.vencedor !== null) {
          this.terminado = true;
          return;
        }
        // Construir a string com as jogadas da diagonal superior que começa na posição (index,index2) 
        let diagonal = this.getCoordenadasDiagonalSuperior(index, index2)
                                 .map((x) => this.getCelulaInterna(x[0],x[1]))
                                 .join("");
        // Ver se alguém venceu na diagonal superior.
        this.vencedor = diagonal.includes("falsefalsefalsefalse") ? ResultadoJogo.AMARELO :
                        (diagonal.includes("truetruetruetrue") ? ResultadoJogo.VERMELHO : null);
        // Se houver um vencedor, o jogo terminou.
        if (this.vencedor !== null) {
          this.terminado = true;
          return;
        }
        // DONE: Implementar a verificação da diagonal inferior.
        // Construir a string para a diagonal inferior que começa na posição (index,index2) 
        let diagonalInf = this.getCoordenadasDiagonalInferior(index, index2)
                                  .map((x) => this.getCelulaInterna(x[0], x[1]))
                                  .join("");
        // Ver se alguém venceu na diagonal inferior.
        this.vencedor = diagonalInf.includes("falsefalsefalsefalse") ? ResultadoJogo.AMARELO :
                        (diagonalInf.includes("truetruetruetrue") ? ResultadoJogo.VERMELHO : null);
        // Se houver um vencedor, o jogo terminou.
        if (this.vencedor !== null) {
          this.terminado = true;
          return;
        }
      }
    }
    // Se o tabuleiro estiver cheio, o jogo terminou empatado.
    this.terminado = this.tabuleiroCheio();
    this.vencedor = this.terminado ? ResultadoJogo.EMPATE : null;
  }

  /*
  A função getCelula(coluna, linha) recebe um inteiro coluna e um inteiro linha e devolve o valor da célula correspondente. Se a coluna ou a linha não forem válidas, ou não existir uma jogada na célula, a função devolve 'null' / 'undefined'. */
  getCelula(coluna, linha) {
    let linhaInversa = Jogo.NUMERO_LINHAS-linha-1;
    return this.getCelulaInterna(coluna, linhaInversa);
  }

  /* 
  A função getCelulaInterna(coluna, linha) recebe um inteiro coluna e um inteiro linha e devolve o valor da célula correspondente. Se a coluna ou a linha não forem válidas, a função devolve 'null'. Se ainda não existir uma jogada na célula, devolve 'undefined'.
  */
  getCelulaInterna(coluna, linha) {
    return coluna < 0 || coluna >= Jogo.NUMERO_COLUNAS || linha < 0 || linha >= Jogo.NUMERO_LINHAS ?
        'null' :
        this.tabuleiro[coluna][linha];
  }

  /*
  A função getCoordenadasLinha recebe as coordenadas de uma coluna e uma linha e retorna um array com as coordenadas das 4 células que formam uma linha para a direita.
  */
  getCoordenadasLinha(coluna, linha) {
    return  [[coluna, linha], [coluna+1, linha], [coluna+2, linha], [coluna+3, linha]];
  }
  
  /*
  A função getCoordenadasColuna recebe as coordenadas de uma coluna e uma linha e retorna um array com as coordenadas das 4 células que formam uma coluna para cima.
  */
  getCoordenadasColuna(coluna, linha) {
    return [[coluna, linha], [coluna, linha+1], [coluna, linha+2], [coluna, linha+3]];
  }
  
  /*
  A função getCoordenadasDiagonalSuperior recebe as coordenadas de uma coluna e uma linha e retorna um array com as coordenadas das 4 células que formam uma diagonal no sentido ascendente da esquerda para a direita.
  */
  getCoordenadasDiagonalSuperior(coluna, linha) {
    return [[coluna, linha], [coluna+1, linha+1], [coluna+2, linha+2], [coluna+3, linha+3]];
  }
  
  /*
  A função getCoordenadasDiagonalInferior recebe as coordenadas de uma coluna e uma linha e retorna um array com as coordenadas das 4 células que formam uma diagonal no sentido descendente da esquerda para a direita.
  */
  getCoordenadasDiagonalInferior(coluna, linha) {
    return [[coluna, linha], [coluna+1, linha-1], [coluna+2, linha-2], [coluna+3, linha-3]];
  }
  
  /*
  A função tabuleiroCheio() verifica se o tabuleiro está cheio.
  O tabuleiro está cheio se todas as colunas estiverem cheias.
  */
  tabuleiroCheio() {
    for (let coluna of this.tabuleiro) {
      if (coluna.length < Jogo.NUMERO_LINHAS) {
        return false;
      }
    }
    return true;
  }

  /*
  A função reset() reinicia o jogo.
  O tabuleiro fica vazio, o jogo não terminou, o jogador a jogar é escolhido aleatoriamente e o vencedor é ResultadoJogo.EMPATE (valor irrelevante por agora).
  */
  reset() {
    this.tabuleiro = [];
    for (let i = 0; i < Jogo.NUMERO_COLUNAS; i++) {
      this.tabuleiro.push([]);
    }
    this.terminado = false;
    this.jogadorAJogar = Math.round(Math.random()) ? false : true;
    this.vencedor = ResultadoJogo.EMPATE;

    console.log(this.tabuleiro);
  }
}