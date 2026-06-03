#let accent      = rgb("#2c3e50")
#let accent-mid  = rgb("#5d8aa8")
#let accent-pale = rgb("#eaf0f6")

#let lang-color(lang) = {
  if lang == "cpp" or lang == "c"               { rgb("#c0392b") }
  else if lang == "xml"                         { rgb("#2471a3") }
  else if lang == "bash" or lang == "sh"        { rgb("#1a7a40") }
  else                                          { accent }
}

#let lang-bg(lang) = {
  if lang == "cpp" or lang == "c"               { rgb("#fdf2f1") }
  else if lang == "xml"                         { rgb("#eef4fb") }
  else if lang == "bash" or lang == "sh"        { rgb("#eef8f3") }
  else                                          { luma(245) }
}


  
  



#let project(body) = {
  set document(title: "Relatório 3ª Fase — Computação Gráfica")
  set page(paper: "a4", margin: (x: 2.6cm, y: 2.6cm))
  set text(font: "New Computer Modern", size: 11pt, lang: "pt")
  set par(justify: true, leading: 0.72em)
  set heading(numbering: "1.")
  set list(indent: 1em)

  // ── Headings ──────────────────────────────────────────────────────────────
  show heading.where(level: 1): it => block(above: 1.6em, below: 0.6em)[
    #set text(fill: accent, size: 14pt, weight: "bold")
    #it
    #v(2pt)
    #line(length: 100%, stroke: 0.8pt + accent)
  ]

  show heading.where(level: 2): it => block(above: 1.1em, below: 0.45em)[
    #set text(fill: accent, size: 12pt, weight: "bold")
    #it
  ]

  show heading.where(level: 3): it => block(above: 0.8em, below: 0.3em)[
    #set text(fill: accent-mid, size: 11pt, weight: "bold")
    #it
  ]

  // ── Block code boxes (tema claro) ────────────────────────────────────────
  show raw.where(block: true): it => {
    let lang-str  = if it.lang != none { upper(it.lang) } else { "CODE" }
    let lang      = if it.lang != none { it.lang } else { "" }
    let color     = lang-color(lang)

    let bg-header = luma(230)
    let bg-body   = lang-bg(lang)

    v(0.7em)
    // ── barra superior ──────────────────────────────────────────────────────
    block(
      fill:   bg-header,
      width:  100%,
      inset:  (x: 11pt, y: 7pt),
      radius: (top-left: 6pt, top-right: 6pt),
      below:  0pt,
    )[
      #grid(
        columns: (auto, 1fr, auto),
        align: horizon,
        text(fill: luma(160), size: 10pt)[● ● ●],
        [],
        // etiqueta da linguagem
        box(
          fill:   color,
          inset:  (x: 7pt, y: 3pt),
          radius: 3pt,
        )[
          #text(fill: white, size: 7.5pt, weight: "bold",
                font: "New Computer Modern Mono")[#lang-str]
        ],
      )
    ]
    // ── corpo do código ─────────────────────────────────────────────────────
    block(
      fill:   bg-body,
      width:  100%,
      stroke: (left: (if lang == "cpp" or lang == "c" { 4pt } else { 3pt }) + color),
      radius: (bottom-left: 6pt, bottom-right: 6pt),
      inset:  (x: 14pt, y: 13pt),
      above:  0pt,
    )[
      #set text(font: "New Computer Modern Mono", size: 9pt, fill: luma(30))
      #it
    ]
    v(0.7em)
  }

  // ── Inline code ───────────────────────────────────────────────────────────
  show raw.where(block: false): it => box(
    fill:   accent-pale,
    stroke: 0.4pt + luma(200),
    inset:  (x: 4pt, y: 2pt),
    radius: 3pt,
    text(font: "New Computer Modern Mono", size: 0.88em, it),
  )

  // ── Figure captions ───────────────────────────────────────────────────────
  show figure.caption: it => text(size: 9.5pt, style: "italic")[#it]

  // ── Tables ────────────────────────────────────────────────────────────────
  set table(
    stroke: 0.5pt + luma(200),
    fill:   (_, y) => if y == 0 { accent-pale }
                      else if calc.odd(y) { luma(252) }
                      else { white },
    inset:  (x: 8pt, y: 6pt),
  )
  show table.cell.where(y: 0): it => {
    set text(weight: "bold", fill: accent, size: 10pt)
    it
  }

  body
}
