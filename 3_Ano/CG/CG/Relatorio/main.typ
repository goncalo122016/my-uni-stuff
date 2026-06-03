#import "cover.typ": cover
#import "template.typ": *
#import "resumo.typ": resumo

#let img-placeholder(desc: "", caption: []) = figure(
  rect(width: 100%, height: 5cm, fill: luma(238),
    stroke: (paint: luma(185), thickness: 1pt, dash: "dashed"), radius: 4pt,
  )[#align(center + horizon)[#text(fill: luma(135), size: 9pt, style: "italic",
      font: "New Computer Modern")[#desc]]],
  caption: caption,
)

#let algo(caption: [], body) = figure(
  block(
    width: 100%, fill: luma(248),
    stroke: (left: 3pt + luma(160)),
    inset: (left: 14pt, top: 10pt, bottom: 10pt, right: 10pt),
    body,
  ),
  caption: caption,
)

#show: project

#cover(
  title: "Trabalho Prático — Fase 4\nGrupo 31",
  authors: (
    (name: "Afonso Martins",  number: "a106931"),
    (name: "Gonçalo Castro", number: "a107337"),
    (name: "Francisco Lage", number: "a106813"),
    (name: "Ricardo Neves",  number: "a106850"),
  ),
  "Maio, 2026",
)

#set page(numbering: "1", number-align: center)
#counter(page).update(1)

#resumo

#pagebreak()

#show outline: it => {
  show heading: set text(size: 14pt)
  it
}
#{
  show outline.entry.where(level: 1): it => { v(5pt); strong(it) }
  outline(title: [Índice])
}

#pagebreak()

// ═══════════════════════════════════════════════════════════════════════════════
= Introdução

O presente relatório descreve o trabalho realizado na *4ª Fase do Trabalho Prático*
da Unidade Curricular *Computação Gráfica*, pertencente ao 2º Semestre do 3º Ano
da Licenciatura em Engenharia Informática, na Universidade do Minho, ano letivo
2024/2025.

Nesta fase foram abordados três tópicos fundamentais que transformam a cena de
geometria simples numa renderização com aspeto realista:

- *Normais e coordenadas de textura*: o gerador passou a produzir, para cada
  vértice, um vetor normal e um par de coordenadas UV. O _engine_ lê e envia
  estes dados para a GPU através de VBOs dedicados, ativando a indexação de
  vértices para eliminar duplicados.

- *Iluminação*: suporte a três tipos de fontes de luz definidas em XML —
  direcional, pontual e _spotlight_ — aplicadas via o modelo de iluminação de
  Phong do OpenGL (_fixed-function pipeline_).

- *Materiais e texturas*: cada modelo pode ter associado um material com
  componentes difusa, ambiente, especular, emissiva e brilho (_shininess_),
  bem como uma textura carregada de ficheiro de imagem e aplicada via
  mapeamento UV.

Como cena de demonstração obrigatória, o sistema solar foi enriquecido com
texturas realistas dos planetas e iluminação a partir do Sol.

// ═══════════════════════════════════════════════════════════════════════════════
= Normais e Coordenadas de Textura

== Formato do ficheiro `.3d`

O formato `.3d` foi estendido para incluir, em cada linha de vértice, o vetor
normal e as coordenadas de textura:

#figure(
  table(
    columns: (auto, 1fr),
    [*Campo*], [*Significado*],
    [`<N>`],      [Número total de vértices no ficheiro],
    [`x y z`],    [Posição do vértice em coordenadas locais],
    [`nx ny nz`], [Vetor normal unitário (opcional — zero se ausente)],
    [`u v`],      [Coordenadas de textura em $[0,1]^2$ (opcional)],
  ),
  caption: [Estrutura de cada linha do ficheiro `.3d` estendido]
)

Os campos de normal e UV são opcionais — se ausentes, o _engine_ usa zeros,
mantendo compatibilidade com ficheiros gerados nas fases anteriores.

== Geração de normais por primitiva

Cada primitiva calcula as normais analiticamente, garantindo suavidade e
correção geométrica:

#figure(
  table(
    columns: (auto, 1fr),
    [*Primitiva*], [*Normal por vértice*],
    [Plano],
      [$bold(n) = (0, 1, 0)$ — constante em toda a superfície],
    [Esfera],
      [$bold(n) = bold(p) \/ r$ — direção radial (posição normalizada)],
    [Cone],
      [$bold(n) = (h sin alpha \/ l,; r\/l,; h cos alpha \/ l)$ onde $l = sqrt(r^2+h^2)$],
    [Cilindro],
      [$bold(n) = (sin alpha, 0, cos alpha)$ nas laterais; $±(0,1,0)$ nas bases],
    [Caixa],
      [$bold(n) = $ normal da face: $(±1,0,0)$, $(0,±1,0)$ ou $(0,0,±1)$],
    [Patch Bézier],
      [Produto vetorial das derivadas parciais $partial cal(P)\/partial u times partial cal(P)\/partial v$],
  ),
  caption: [Cálculo de normais por primitiva geométrica]
)

Para a esfera em coordenadas esféricas $(alpha, beta)$ (longitude, latitude):

$ bold(n)(alpha, beta) = (cos beta cos alpha,; sin beta,; cos beta sin alpha) $

Para o cone de raio $r$, altura $h$ e comprimento de geratriz $l = sqrt(r^2+h^2)$,
a normal na lateral ao ângulo azimutal $alpha$ é:

$ bold(n)(alpha) = (h/l sin alpha,; r/l,; h/l cos alpha) $

Para os _patches_ de Bézier bicúbicos, a posição na superfície e a normal são:

$ cal(P)(u,v) = sum_(i=0)^(3) sum_(j=0)^(3) B_i(u)\, B_j(v)\, bold(P)_(i j),
  quad bold(n)(u,v) = frac(partial cal(P), partial u) times frac(partial cal(P), partial v) $

== Coordenadas UV

As coordenadas de textura $(u,v) in [0,1]^2$ mapeiam a superfície de cada
primitiva para o espaço de textura:

#figure(
  table(
    columns: (auto, auto, auto),
    [*Primitiva*], [*$u$*], [*$v$*],
    [Esfera],  [$alpha \/ (2pi)$],          [$(beta + pi\/2) \/ pi$],
    [Cone],    [ângulo azimutal $\/ (2pi)$], [$y \/ h$],
    [Plano],   [$x \/ L$],                   [$z \/ L$],
    [Caixa],   [por face],                   [cada face mapeada para $[0,1]^2$],
  ),
  caption: [Fórmulas de coordenadas de textura por primitiva]
)

Na esfera, a projeção esférica equiretangular desenvolve a superfície como um
mapa-mundo: $u$ percorre a longitude e $v$ a latitude, de polo a polo.

```cpp
Vec3f norm = pos.Normalized();          // normal = posição normalizada
float u    = alpha / (2.0f * M_PI);    // longitude → [0, 1]
float v    = (beta + M_PI_2) / M_PI;   // latitude  → [0, 1]
```

== Indexação de vértices

O _engine_ elimina vértices duplicados usando uma tabela de hash cujas chaves
incluem todos os atributos do vértice $(x, y, z, n_x, n_y, n_z, u, v)$.
O resultado é um _index buffer_ compacto que permite usar `glDrawElements` em
vez de `glDrawArrays`, reduzindo o volume de dados enviados à GPU.

#algo(caption: [Desduplicação de vértices com tabela de hash])[
  *Entrada:* sequência de vértices $V = {(bold(p), bold(n), u, v)}$ lida do ficheiro `.3d`\
  *Saída:* arrays $P$, $N$, $"UV"$ (únicos) e buffer de índices $I$

  #v(5pt)
  $H arrow.l emptyset$ #h(6pt) _(tabela de hash: $(bold(p), bold(n), u, v) arrow.r$ índice)_\
  *Para cada* vértice $v$ em $V$:\
  #h(2em) *Se* $v in H$:\
  #h(4em) $I."adicionar"(H[v])$ #h(12pt) _(vértice já existe — reutiliza o índice)_\
  #h(2em) *Senão*:\
  #h(4em) $"idx" arrow.l |P|$\
  #h(4em) $P."adicionar"(v.bold(p))$; $N."adicionar"(v.bold(n))$; $"UV"."adicionar"((v.u, v.v))$\
  #h(4em) $H[v] arrow.l "idx"$; #h(6pt) $I."adicionar"("idx")$\
  *Renderizar* com `glDrawElements(I)`
]

Os dados geométricos são organizados em quatro VBOs independentes:

#figure(
  table(
    columns: (auto, auto, 1fr),
    [*VBO*], [*Tipo OpenGL*], [*Conteúdo*],
    [`vbo_pos`],  [`GL_ARRAY_BUFFER`],         [Posições $(x,y,z)$ — `GL_FLOAT × 3`],
    [`vbo_norm`], [`GL_ARRAY_BUFFER`],         [Normais $(n_x, n_y, n_z)$ — `GL_FLOAT × 3`],
    [`vbo_tex`],  [`GL_ARRAY_BUFFER`],         [Coordenadas UV $(u,v)$ — `GL_FLOAT × 2`],
    [`vbo_idx`],  [`GL_ELEMENT_ARRAY_BUFFER`], [Índices de vértice — `GL_UNSIGNED_INT`],
  ),
  caption: [Organização dos VBOs por tipo de dado geométrico]
)



// ═══════════════════════════════════════════════════════════════════════════════
= Iluminação

A iluminação é implementada através do modelo de Phong do _fixed-function
pipeline_ do OpenGL. O XML permite definir até 8 fontes de luz (limite do
OpenGL), de três tipos diferentes.

== Modelo de iluminação de Phong

A intensidade resultante num ponto da superfície é dada pela soma das
contribuições de todas as $N$ fontes de luz:

$ I = bold(k)_e + bold(k)_a dot bold(I)_a
    + sum_(j=1)^(N) lr([bold(k)_d (bold(l)_j dot bold(n))
    + bold(k)_s (bold(r)_j dot bold(v))^s]) $

#figure(
  table(
    columns: (auto, 1fr),
    [$bold(k)_e$], [Componente emissiva — independente de qualquer fonte de luz],
    [$bold(k)_a, bold(I)_a$], [Componente ambiente e iluminação global do ambiente],
    [$bold(k)_d$], [Componente difusa — proporcional a $cos theta = bold(l)_j dot bold(n)$ (Lei de Lambert)],
    [$bold(k)_s$], [Componente especular — reflexo de Phong com expoente $s$],
    [$bold(r)_j$], [Vetor de reflexão da luz $j$ em relação à normal $bold(n)$],
    [$bold(v)$],   [Vetor de visão (direção para a câmara)],
  ),
  caption: [Termos do modelo de iluminação de Phong]
)

== Definição em XML

```xml
<lights>
  <light type="directional" dirX="1" dirY="0.7" dirZ="0.5" />
  <light type="point"       posX="0" posY="10"  posZ="0"   />
  <light type="spot"        posX="0" posY="10"  posZ="0"
                            dirX="0" dirY="-1"  dirZ="0"
                            cutoff="30" />
</lights>
```

O _parser_ aceita tanto os atributos em _camelCase_ (`posX`, `dirX`) como em
minúsculas (`posx`, `dirx`) para compatibilidade com diferentes versões dos
ficheiros de teste.

== Tipos de luz

=== Luz direcional

Simula uma fonte infinitamente distante (ex.: Sol numa cena exterior). A
direção é constante em toda a cena e a intensidade não sofre atenuação com a
distância. O OpenGL distingue os tipos de luz pelo quarto componente $w$ da
posição homogénea enviada a `GL_POSITION`:

$ bold(p)_h = mat(d_x; d_y; d_z; w = 0) quad arrow.r.double quad "fonte no infinito" $

```cpp
float dir4[4] = {dl.dir.x, dl.dir.y, dl.dir.z, 0.0f};
glLightfv(GL_LIGHT0 + i, GL_POSITION, dir4);
```

=== Luz pontual

Emite luz em todas as direções a partir de uma posição finita no espaço.
Com $w = 1$, o OpenGL trata o vetor como posição:

$ bold(p)_h = mat(p_x; p_y; p_z; w = 1) quad arrow.r.double quad "fonte em posição finita" $

```cpp
float pos4[4] = {pl.pos.x, pl.pos.y, pl.pos.z, 1.0f};
glLightfv(GL_LIGHT0 + i, GL_POSITION, pos4);
```

A intensidade decresce com a distância $d$ segundo a atenuação quadrática:

$ I(d) = I_0 / (k_c + k_l d + k_q d^2) $

onde $k_c$, $k_l$ e $k_q$ são os coeficientes constante, linear e quadrático,
respetivamente.

=== _Spotlight_

Fonte pontual com cone de iluminação limitado pelo ângulo de _cutoff_ $phi_c$.
Um ponto é iluminado apenas se o ângulo $theta$ entre o vetor de incidência
$bold(l)$ e a direção do _spot_ $bold(d)$ satisfaz:

$ theta = arccos(bold(l) dot bold(d)) <= phi_c $

Dentro do cone, a intensidade é ainda modulada pelo expoente de _spotlight_:

$ I_"spot" = (bold(l) dot bold(d))^("expoente") times I_"pontual" $

#figure(
  table(
    columns: (auto, auto, auto, auto),
    [*Tipo*], [*Parâmetros XML*], [*$w$*], [*Atenuação*],
    [Direcional], [`dirX dirY dirZ`],                         [`0`], [Nenhuma],
    [Pontual],    [`posX posY posZ`],                         [`1`], [Quadrática com a distância],
    [Spotlight],  [`posX posY posZ dirX dirY dirZ cutoff`],   [`1`], [Quadrática + cone angular],
  ),
  caption: [Comparação dos três tipos de luz suportados]
)




// ═══════════════════════════════════════════════════════════════════════════════
= Materiais

Cada modelo pode ter um material com cinco componentes definidas em XML:

```xml
<color>
  <diffuse  R="0"   G="87"  B="184" />
  <ambient  R="0"   G="8"   B="18"  />
  <specular R="255" G="255" B="255" />
  <emissive R="0"   G="0"   B="0"   />
  <shininess value="128" />
</color>
```

Os valores RGB são definidos em inteiros $[0, 255]$ e convertidos para
componentes _float_ $[0.0, 1.0]$ antes de serem enviados ao OpenGL:

$ c_i = R_i / 255, quad i in {R, G, B}, quad c_alpha = 1.0 $

As cinco componentes têm os seguintes papéis no modelo de Phong:

#figure(
  table(
    columns: (auto, 1fr),
    [*Componente*], [*Papel no modelo de Phong*],
    [Difusa $bold(k)_d$],   [Cor base — proporcional ao cosseno do ângulo de incidência (Lei de Lambert)],
    [Ambiente $bold(k)_a$], [Iluminação mínima independente da direção da luz],
    [Especular $bold(k)_s$],[Reflexo brilhante dependente da posição do observador],
    [Emissiva $bold(k)_e$], [Luz própria do objeto — soma-se independentemente de qualquer fonte],
    [Brilho $s$],           [Expoente de Phong $s in [0, 128]$ — controla a largura do reflexo especular],
  ),
  caption: [Componentes do modelo de material de Phong]
)

Para evitar chamadas redundantes ao driver a cada _frame_, o _engine_ mantém
um cache do último material enviado à GPU e só o reenvia quando este muda:

#algo(caption: [Cache de estado de material — minimiza chamadas ao driver])[
  *Estado:* $M_"cache"$ ← último material enviado; válido: booleano

  #v(5pt)
  *Para cada modelo* a renderizar:\
  #h(2em) *Se* $M_"cache"$ inválido *ou* $M_"atual" eq.not M_"cache"$:\
  #h(4em) Enviar difusa, ambiente, especular, emissiva e brilho para a GPU\
  #h(4em) $M_"cache" arrow.l M_"atual"$\
  #h(2em) *Senão*:\
  #h(4em) _(nenhuma chamada ao driver — estado GPU já é o correto)_
]

 #figure(image("images/4_3.png", width: 80%), caption: [Materiais com componente especular — _shininess_ = 128])
#v(2em)


// ═══════════════════════════════════════════════════════════════════════════════
= Texturas

== Carregamento

As texturas são carregadas a partir de ficheiros de imagem (JPEG, PNG, etc.)
usando a biblioteca _stb\_image_. O processo segue os seguintes passos:

+ *Inversão vertical*: a origem do espaço UV do OpenGL está no canto
  inferior-esquerdo, mas a maioria dos ficheiros de imagem usa o canto
  superior-esquerdo. A coordenada $v$ é invertida na leitura para corrigir
  esta discrepância.

+ *Descodificação*: a imagem é lida com resolução $w times h$ e $c in {3, 4}$
  canais (RGB ou RGBA).

+ *Geração do _texture object_*: é criado um identificador de textura na GPU.

+ *Configuração dos filtros de amostragem*:
  - _Minificação_: trilinear com _mipmaps_ (`GL_LINEAR_MIPMAP_LINEAR`) — elimina _aliasing_ em objetos distantes.
  - _Magnificação_: bilinear (`GL_LINEAR`) — suaviza a ampliação.

+ *Modo de repetição*: `GL_REPEAT` em ambos os eixos $S$ e $T$.

+ *Transferência para a GPU*: os pixeis são enviados para memória de vídeo
  no formato RGB ou RGBA conforme $c$.

+ *Geração de _mipmaps_*: a cadeia completa de _mipmaps_ é gerada
  automaticamente a partir do nível base.

== Cache de texturas

As texturas são partilhadas entre todos os modelos que referenciam o mesmo
ficheiro — o caminho do ficheiro serve como chave de cache. A transferência
para a GPU ocorre apenas na primeira utilização:

#algo(caption: [Cache de texturas — partilha entre instâncias])[
  *Estado:* $T_"cache"$: mapa (caminho $arrow.r$ identificador GPU)

  #v(5pt)
  *Ao carregar textura* com caminho $p$:\
  #h(2em) *Se* $p in T_"cache"$:\
  #h(4em) Reutilizar $T_"cache"[p]$ #h(8pt) _(sem nova transferência para a GPU)_\
  #h(2em) *Senão*:\
  #h(4em) Executar passos 1–7 do algoritmo de carregamento\
  #h(4em) $T_"cache"[p] arrow.l$ identificador gerado
]

== Definição em XML

A textura é associada a cada modelo individualmente; o material serve de
_fallback_ caso o ficheiro de textura não seja encontrado:

```xml
<model file="assets/models/sphere.3d">
  <texture file="2k_earth_daymap.jpg" />
  <color>
    <diffuse R="200" G="200" B="200" />
    <ambient R="50"  G="50"  B="50"  />
  </color>
</model>
```

O _engine_ procura o ficheiro em múltiplos caminhos predefinidos
(`assets/textures/`, caminhos relativos ao binário, etc.), garantindo
portabilidade independente do diretório de execução.

== Aplicação na renderização

Durante a renderização, o _binding_ de textura é controlado por um cache de
estado à semelhança do cache de material: a chamada `glBindTexture` é emitida
apenas quando a textura muda entre modelos consecutivos.

Os estados OpenGL necessários — `GL_TEXTURE_2D`, `GL_VERTEX_ARRAY`,
`GL_NORMAL_ARRAY` e `GL_TEXTURE_COORD_ARRAY` — são ativados uma única vez
durante a inicialização e permanecem ativos durante toda a sessão, evitando
chamadas redundantes a `glEnable`/`glEnableClientState` por _frame_.


 #figure(image("images/4_6.png", width: 80%), caption: [Cena com texturas aplicadas via mapeamento UV])
#v(1em)

// ═══════════════════════════════════════════════════════════════════════════════
= Testes

Os seis testes desta fase validam progressivamente a iluminação, os materiais
e as texturas.

== test\_4\_1.xml — Luz direcional, cor difusa

Cena mínima com um cubo iluminado por uma única luz direcional com direção
$(1, 0.7, 0.5)$ e material de difusa amarela, sem componentes especular ou
emissiva. A variação de intensidade Lambertiana entre as faces torna visível
a orientação de cada face em relação à fonte de luz.

#figure(image("images/4_1.png", width: 80%), caption: [test\_4\_1.xml — cubo com luz direcional e material difuso amarelo])
#v(2em)



== test\_4\_2.xml — Componente emissiva

Idêntico ao anterior mas com componente emissiva vermelha intensa. A emissiva
soma-se ao amarelo difuso resultando num tom laranja mesmo nas faces menos
iluminadas — evidencia que a componente $bold(k)_e$ é independente de qualquer
fonte de luz.

#figure(image("images/4_2.png", width: 80%), caption: [test\_4\_2.xml — efeito da componente emissiva na cor final])
#v(2em)



== test\_4\_3.xml — Dois _point lights_, especular

Plano com cone, esfera, bule e caixa com material azul e _shininess_ = 128,
iluminados por dois pontos de luz em posições simétricas. O expoente de Phong
elevado produz reflexos especulares concentrados e brilhantes nos objetos curvos.


#figure(image("images/4_3.png", width: 80%), caption: [test\_4\_3.xml — reflexos especulares com dois _point lights_])
#v(2em)


== test\_4\_4.xml — _Point light_ central

Mesma cena mas com uma única luz pontual muito próxima da origem $(0, 0.2, 0)$.
A atenuação quadrática $I(d) prop 1/d^2$ produz contraste dramático —
objetos próximos da fonte ficam muito mais brilhantes que os afastados.

#figure(image("images/4_4.png", width: 80%), caption: [test\_4\_4.xml — atenuação quadrática com _point light_ central])
#v(2em)


== test\_4\_5.xml — _Spotlight_

_Spotlight_ com ângulo de _cutoff_ $phi_c = 10°$ posicionado acima da cena
e apontando para a origem. Apenas os objetos dentro do cone ($theta <= phi_c$)
recebem iluminação — as bordas da cena ficam completamente escuras.

#figure(image("images/4_5.png", width: 80%), caption: [test\_4\_5.xml — _spotlight_ com _cutoff_ de 10°])
#v(2em)


== test\_4\_6.xml — Texturas

Mesma disposição de objetos mas com texturas em todos os modelos: relva no
plano, cone de trânsito, globo terrestre, bule cerâmico e caixote de madeira.
Dois _point lights_ garantem iluminação suficiente para as texturas serem
visíveis com os seus detalhes e cores.

#figure(image("images/4_6.png", width: 80%), caption: [test\_4\_6.xml — cena com texturas e iluminação])
#v(2em)




// ═══════════════════════════════════════════════════════════════════════════════
= Sistema Solar

O sistema solar foi enriquecido com texturas realistas e iluminação a partir
do Sol, mantendo todas as animações da Fase 3.

== Iluminação solar

O sistema solar usa quatro fontes de luz complementares, definidas no XML:

#figure(
  table(
    columns: (auto, auto, 1fr),
    [*Tipo*], [*Posição / Direção*], [*Papel*],
    [Pontual],      [origem $(0,0,0)$ — Sol],        [Luz principal, amarela quente; simula a radiação solar com atenuação quadrática],
    [Direcional],   [$(1,\;0.3,\;0.5)$],             [Luz de preenchimento fria para iluminar ligeiramente o lado escuro dos planetas],
    [_Spotlight_],  [perto de Saturno, cutoff = 20°],[Feixe azul-branco direcionado a Saturno e aos seus anéis],
    [Pontual],      [$(-200,\;50,\;-200)$],           [_Rim light_ de fundo para evitar silhuetas completamente negras],
  ),
  caption: [Fontes de luz do sistema solar]
)

A luz pontual do Sol é a dominante: a sua atenuação quadrática com a distância
produz um gradiente de brilho realista do interior para o exterior do sistema.
As restantes três luzes são auxiliares e de baixa intensidade, garantindo que
nenhuma face fica completamente negra e destacando visualmente Saturno.

== Estrutura hierárquica de cada planeta

Cada planeta é representado por dois grupos aninhados, separando as
transformações de órbita e de auto-rotação:

#figure(
  table(
    columns: (auto, 1fr),
    [*Grupo*], [*Transformações aplicadas*],
    [Exterior], [Rotação orbital em torno do Sol + translação para a distância orbital],
    [Interior], [Auto-rotação em torno do próprio eixo, com período independente da órbita],
  ),
  caption: [Estrutura de dois grupos aninhados por planeta]
)

Esta separação permite animar órbita e rotação independentemente, sem
acumulação de erros de ponto flutuante.

== Texturas dos corpos celestes

#figure(
  table(
    columns: (auto, auto),
    [*Corpo celeste*], [*Ficheiro de textura*],
    [Skybox (Via Láctea)], [`2k_stars_milky_way.jpg` — esfera invertida emissiva a envolver toda a cena],
    [Sol],               [`2k_sun.jpg` — material emissivo amarelo],
    [Mercúrio],          [`2k_mercury.jpg`],
    [Vénus],             [`2k_venus_surface.jpg`],
    [Terra],             [`2k_earth_daymap.jpg`],
    [Lua (Terra)],       [`2k_moon.jpg`],
    [Marte],             [`2k_mars.jpg`],
    [Fobos / Deimos],    [`2k_moon.jpg`],
    [Júpiter],           [`2k_jupiter.jpg`],
    [Luas de Júpiter],   [`2k_moon.jpg` (2 luas)],
    [Saturno],           [`2k_saturn.jpg`],
    [Anel de Saturno],   [`saturnringcolor.jpg` — torus com textura de anel],
    [Titan],             [`2k_moon.jpg`],
    [Urano],             [sem textura — material azul-ciano com componente emissiva],
    [Anel de Urano],     [`uranusringcolour.jpg` — inclinado 98° sobre o eixo Z],
    [Neptuno],           [`neptunemap.jpg`],
    [Plutão],            [`plutomap1k.jpg`],
    [Cometa],            [`2k_moon.jpg` — material com componente emissiva azul-branca],
  ),
  caption: [Texturas associadas a cada corpo celeste do sistema solar]
)

== Estrutura XML (exemplo: Terra)

```xml
<group>
  <transform>
    <rotate time="10" x="0" y="1" z="0" />  <!-- órbita anual -->
  </transform>
  <transform>
    <translate x="19" y="0" z="0" />         <!-- distância ao Sol -->
  </transform>
  <group>
    <transform>
      <rotate time="2" x="0" y="1" z="0" />  <!-- rotação diária -->
    </transform>
    <models>
      <model file="assets/models/sphere.3d">
        <texture file="2k_earth_daymap.jpg" />
        <color>
          <diffuse  R="120" G="160" B="220" />
          <ambient  R="30"  G="40"  B="55"  />
          <specular R="150" G="160" B="200" />
          <shininess value="80" />
        </color>
      </model>
    </models>
  </group>
</group>
```

Os dois blocos `<transform>` são intencionais: o primeiro aplica a rotação
orbital (antes de qualquer translação), o segundo posiciona o planeta à
distância correta. Esta separação resulta em órbitas circulares centradas
na origem sem acumulação de erros de composição de matrizes.

#img-placeholder(
  desc: "Screenshot geral do sistema solar: skybox da Via Láctea a envolver a cena; Sol emissivo ao centro; Mercúrio, Vénus, Terra (com Lua), Marte (com Fobos e Deimos), Júpiter (com 2 luas), Saturno (com anel e Titan), Urano (com anel inclinado), Neptuno e Plutão em órbita. Feixe de _spotlight_ azul-branco visível sobre Saturno. Gradiente de brilho do interior (planetas mais próximos) para o exterior evidencia a atenuação quadrática da luz pontual do Sol.",
  caption: [Sistema solar completo — skybox, 8 planetas + Plutão, luas, anéis e 4 fontes de luz],
)

#img-placeholder(
  desc: "Screenshot aproximado da Terra e Lua: textura equiretangular `2k_earth_daymap.jpg` com continentes visíveis; hemisfério voltado ao Sol bem iluminado e face noturna escura com reflexo especular nos oceanos (shininess = 80). Lua em órbita com textura `2k_moon.jpg` e iluminação lambertiana.",
  caption: [Terra e Lua — mapeamento UV equiretangular e iluminação solar],
)

// ═══════════════════════════════════════════════════════════════════════════════
= Otimizações

Além das funcionalidades obrigatórias, foram implementadas várias otimizações
que melhoram significativamente o desempenho em cenas complexas.

== Cache partilhada de VBOs e texturas

Modelos e texturas referenciados múltiplas vezes na cena (ex.: `sphere.3d`
usado por todos os planetas) são carregados para a GPU uma única vez. As
instâncias subsequentes reutilizam os mesmos identificadores de VBO e textura,
sem nova transferência de dados.

#figure(
  table(
    columns: (auto, auto, auto),
    [*Recurso*], [*Primeiro acesso (cache miss)*], [*Acessos seguintes (cache hit)*],
    [Geometria], [Ler `.3d`, criar e preencher 4 VBOs na GPU], [Copiar referências dos VBOs já existentes],
    [Textura],   [Descodificar imagem, transferir para GPU],    [Reutilizar identificador GPU],
  ),
  caption: [Comportamento da cache em primeiro e segundo acesso ao mesmo recurso]
)

No sistema solar, os 8 planetas partilham o mesmo VBO de esfera — a geometria
é enviada para a GPU apenas uma vez, independentemente do número de instâncias.

== Cache de estado GPU

Para cada tipo de recurso ligável (VBOs, textura, material), o _engine_ mantém
o identificador do último valor enviado ao driver. Uma chamada de _binding_ ou
atualização só é emitida quando o valor muda entre modelos consecutivos:

#figure(
  table(
    columns: (auto, 1fr),
    [*Cache*], [*Evita chamada redundante ao driver*],
    [VBO de posições],   [`glBindBuffer` + `glVertexPointer` quando mesmo modelo é consecutivo],
    [VBO de normais],    [`glBindBuffer` + `glNormalPointer` idem],
    [VBO de coordenadas],[`glBindBuffer` + `glTexCoordPointer` idem],
    [Buffer de índices], [`glBindBuffer` do `GL_ELEMENT_ARRAY_BUFFER` idem],
    [Textura],           [`glBindTexture` entre modelos com a mesma textura],
    [Material],          [`glMaterialfv` entre modelos com o mesmo material],
  ),
  caption: [Caches de estado GPU mantidas pelo _engine_ por _frame_]
)

== _Frustum culling_ hierárquico

O _engine_ calcula uma AABB (_Axis-Aligned Bounding Box_) em coordenadas
mundo para cada grupo da hierarquia de cena e testa-a contra os seis planos do
_frustum_ de visão. Grupos completamente fora de algum plano são descartados
inteiramente, incluindo todos os seus descendentes, sem emitir qualquer _draw
call_.

A técnica de _plane masking_ (máscara de planos) reduz adicionalmente o número
de testes: quando a AABB de um grupo está completamente dentro de um plano,
esse plano é removido da máscara dos descendentes e não é retestado:

#algo(caption: [_Frustum culling_ hierárquico com _plane masking_])[
  *Entrada:* grupo $G$; máscara herdada $M$ (6 bits — um por plano do _frustum_)\
  *Invariante:* bit $M[i] = 1$ indica que o plano $i$ ainda não foi satisfeito

  #v(5pt)
  $M_"filho" arrow.l M$\
  *Para cada* plano $p_i$ com $M[i] = 1$:\
  #h(2em) Testar $"AABB"(G)$ contra $p_i$:\
  #h(4em) _Totalmente fora:_ #h(4pt) retornar #h(8pt) _(G e toda a descendência descartados)_\
  #h(4em) _Totalmente dentro:_ #h(4pt) $M_"filho" [i] arrow.l 0$ #h(8pt) _(plano satisfeito — não retestar)_\
  #h(4em) _Interseta:_ #h(4pt) $M_"filho" [i]$ mantém-se $1$\
  Renderizar modelos de $G$\
  *Para cada* filho $F$ de $G$:\
  #h(2em) $"Render"(F,\, M_"filho")$ #h(8pt) _(máscara reduzida — menos testes por nível)_
]

// ═══════════════════════════════════════════════════════════════════════════════
= Conclusão

Nesta fase foram implementadas com sucesso todas as funcionalidades propostas:

- *Normais e UV*: o gerador produz normais analíticas e coordenadas de textura
  para todas as primitivas. O _engine_ lê estes dados, elimina vértices
  duplicados via tabela de hash e envia para a GPU através de VBOs indexados.

- *Iluminação*: os três tipos de luz (direcional, pontual e _spotlight_) são
  definidos em XML e aplicados via o modelo de Phong, com suporte a até 8
  fontes simultâneas.

- *Materiais*: as cinco componentes de Phong (difusa, ambiente, especular,
  emissiva, brilho) são configuráveis por modelo em XML e aplicadas com cache
  de estado para minimizar chamadas ao driver.

- *Texturas*: carregamento com _stb\_image_ e _mipmaps_ automáticos,
  partilha de texturas entre instâncias e aplicação via mapeamento UV gerado
  analiticamente para cada primitiva.

As otimizações implementadas — cache partilhada de VBOs e texturas, cache de
estado GPU e _frustum culling_ hierárquico com _plane masking_ — permitem manter
taxas de _frame_ elevadas mesmo com o sistema solar completo: 19 corpos celestes
com texturas individuais, anéis de Saturno e Urano, skybox da Via Láctea,
luas animadas e 4 fontes de luz simultâneas.
