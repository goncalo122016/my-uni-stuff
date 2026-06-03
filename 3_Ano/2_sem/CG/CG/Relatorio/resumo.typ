#let resumo = [
  #heading(level: 1, outlined: false)[Resumo]

  Este relatório descreve a quarta e última fase do trabalho prático da unidade
  curricular de Computação Gráfica, na qual o motor de renderização 3D foi
  expandido com suporte a iluminação, materiais e texturas.

  O gerador de primitivas foi estendido para produzir, em cada vértice, um vetor
  normal calculado analiticamente e coordenadas de textura UV. O motor passou a
  ler estes dados e a organizá-los em quatro VBOs independentes — posições,
  normais, coordenadas UV e índices — eliminando vértices duplicados através de
  uma tabela de hash e utilizando `glDrawElements` para a renderização indexada.

  A iluminação foi implementada seguindo o modelo de Phong do _pipeline_
  de função fixa do OpenGL, com suporte a três tipos de fontes de luz
  configuráveis em XML: direcional (fonte no infinito, sem atenuação), pontual
  (atenuação quadrática com a distância) e _spotlight_ (cone de iluminação
  definido por ângulo de _cutoff_). Os materiais, com as componentes difusa,
  ambiente, especular, emissiva e brilho, são igualmente configuráveis por
  modelo em XML. As texturas são carregadas com a biblioteca _stb\_image_ com
  geração automática de _mipmaps_ e partilhadas entre instâncias do mesmo
  ficheiro.

  Como cena de demonstração, o sistema solar foi enriquecido com texturas
  fotorrealistas dos planetas, anéis de Saturno e Urano, luas, um skybox da Via
  Láctea e quatro fontes de luz complementares (luz pontual do Sol, direcional de
  preenchimento, _spotlight_ sobre Saturno e _rim light_ de fundo). Foram ainda
  implementadas otimizações de desempenho: cache partilhada de VBOs e texturas,
  cache de estado GPU para minimizar chamadas redundantes ao driver, e _frustum
  culling_ hierárquico com _plane masking_ para descartar grupos fora do campo
  de visão sem emitir _draw calls_ desnecessários.

  #v(6pt)
  *Palavras-chave:* OpenGL, iluminação de Phong, normais, coordenadas UV,
  texturas, VBOs, _frustum culling_, sistema solar.
]
