from Graph import Graph
from Node import Node


def main():
    g = Graph()

    #Ficha2
    g.add_edge("gualtar", "svitor", 8)
    g.add_edge("svitor", "svicente", 6)
    g.add_edge("svicente", "nogueiro", 8)
    g.add_edge("gualtar", "smamede", 6)
    g.add_edge("smamede", "sobreposta", 3)
    g.add_edge("smamede", "lamacaes", 8)
    g.add_edge("sobreposta", "nogueiro", 6)
    g.add_edge("lamacaes", "fraiao", 3)
    g.add_edge("fraiao", "nogueiro", 6)
    

    #Ficha2
    g.add_heuristica("gualtar", 8)
    g.add_heuristica("svitor", 2)
    g.add_heuristica("svicente", 6)
    g.add_heuristica("nogueiro", 0)
    g.add_heuristica("smamede", 7)
    g.add_heuristica("sobreposta", 4)
    g.add_heuristica("lamacaes", 4)
    g.add_heuristica("fraiao", 3)

    saida = -1
    while saida != 0:
        print("1-Imprimir Grafo")
        print("2-Desenhar Grafo")
        print("3-Imprimir  nodos de Grafo")
        print("4-Imprimir arestas de Grafo")
        print("5-DFS")
        print("6-BFS")
        print("7-A*")
        print("8-Gulosa")
        print("0-Saír")

        saida = int(input("introduza a sua opcao-> "))
        if saida == 0:
            print("saindo.......")
        elif saida == 1:
            print(g.m_graph)
            l = input("prima enter para continuar")
        elif saida == 2:
            g.desenha()
        elif saida == 3:
            print(g.m_graph.keys())
            l = input("prima enter para continuar")
        elif saida == 4:
            print(g.imprime_aresta())
            l = input("prima enter para continuar")
        elif saida == 5:
            inicio = input("Nodo inicial->")
            fim = input("Nodo final->")
            print(g.procura_DFS(inicio, fim, path=[], visited=set()))
            l = input("prima enter para continuar")
        elif saida == 6:
            inicio = input("Nodo inicial->")
            fim = input("Nodo final->")
            print(g.procura_BFS(inicio, fim))
            l = input("prima enter para continuar")
        elif saida == 7:
            inicio = input("Nodo inicial->")
            fim = input("Nodo final->")
            print(g.procura_aStar(inicio, fim))
            l = input("prima enter para continuar")
        elif saida == 8:
            inicio = input("Nodo inicial->")
            fim = input("Nodo final->")
            print(g.greedy(inicio, fim))
            l = input("prima enter para continuar")
        else:
            print("you didn't add anything")
            l = input("prima enter para continuar")


if __name__ == "__main__":
    main()
