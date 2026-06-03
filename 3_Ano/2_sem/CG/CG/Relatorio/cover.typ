#let blue = rgb("31849b")
#let gray = rgb("808080")
#let light_gray = rgb("A6A6A6")

#let cover(title: "", authors: ("A"), string_date) = {
  let render_authors = grid(columns: 2,
                          column-gutter: 15pt,
                          row-gutter: 8pt,
                          ..authors.map(it => [
                            #text(size:12pt, weight: "bold", it.name) \
                            #text(size: 11pt, it.number)
                          ])
                         )

                           
  {
    set page(paper: "a4", margin: (x: 0cm,y: 0cm))
    
    rect(fill: blue,height: 100%, width:23.3%)
    
    place(bottom + left,dx: 70pt,dy:-40pt, {
      text(weight:"bold", size: 120pt, fill: white, [C])
      text(weight:"bold", size: 120pt, fill: blue, [G])
    })
  
    {
      set place(top+left, dx: 200pt)
      place(dy: 120pt, image("images/uminho.svg", height: 8%))
      place(dy: 200pt, {
        text(size: 10pt, weight: "bold", fill: gray, [Universidade do Minho\ ])
        text(size: 9pt, fill: gray, [Escola de Engenharia\ Licenciatura em Engenharia Informática\ ])
      })
      place(dy: 300pt, {
  
        text(size: 20pt, fill: blue, weight: "bold", [Unidade Curricular de \ Computação Gráfica\ ])
        text(size: 10pt, [Ano Letivo de 2025/2026])
      })
      place(dy: 520pt, text(size: 20pt, weight: "bold", title))
      place(dy: 590pt, render_authors)
      
      place(dy: 670pt, text(size: 12pt, string_date))
    }
  }
}