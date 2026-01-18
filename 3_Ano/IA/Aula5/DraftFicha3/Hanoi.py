# Problema das Torres de Hanoi com 4 discos (D1(menor), D2, D3, D4(maior)) e 3 torres (A, B, C)

# Inicialmente os discos encontram-se posicionados na torre C e o objetivo é transferi-los para a torre A

# Em cada movimento, o jogador pode mover um disco de uma torre para outra torre, desde que este não coloque esse disco por cima de um disco menor.

from Graph import Grafo
from nodo import Node
from queue import Queue

class Hanoi():
    

    def __init__(self, start="(C,C,C,C)", goal="(A,A,A,A)"):
        self.g=Grafo(directed=True)
        self.start=start
        self.goal=goal


    # Partindo do estado inicial, utilizando as ações possíveis como transições
    # construir o grafo
    def cria_grafo(self):
        estados = []
        estados.append(self.start)
        visitados = []
        visitados.append(self.start)

        while estados != []:
            estado = estados.pop(0)

            # Expande o estado atual
            novos_estados = self.expande(estado)

            for e in novos_estados:
                self.g.add_edge(estado, e, 1)
                if e not in visitados:
                    visitados.append(e)
                    estados.append(e)

    def expande(self, estado):
        lista = []
        discos = [estado[1], estado[3], estado[5], estado[7]]

        for i in range(4):
            origem = discos[i]
            
            # Verifica se este disco está no topo da sua torre
            disco_no_topo = True
            for j in range(i):  # Verifica discos menores
                if discos[j] == origem:
                    disco_no_topo = False
                    break
            
            if not disco_no_topo:
                continue  # Só pode mover discos no topo
            
            # Verifica para onde o disco pode ser movido
            for destino in ['A', 'B', 'C']:
                if destino != origem:
                    # Verifica se o movimento é válido
                    pode_mover = True
                    
                    # Encontra o disco no topo da torre de destino
                    disco_topo_destino = None
                    for j in range(4):  # Do menor para o maior
                        if discos[j] == destino:
                            disco_topo_destino = j
                            break
                    
                    # Se há um disco na torre de destino e é menor que o atual
                    if disco_topo_destino is not None and disco_topo_destino < i:
                        pode_mover = False
                    
                    if pode_mover:
                        novos_discos = discos.copy()
                        novos_discos[i] = destino
                        novo_estado = "(" + ",".join(novos_discos) + ")"
                        lista.append(novo_estado)        
        return lista
    
    # Encontra a solução utilizando DFS (recorre à classe grafo e node implementada antes
    def solucaoDFS(self,start,goal):
        res=self.g.procura_DFS(start,goal,path=[], visited=set())
        return (res)

    # Encontra a solução utilizando BFS (recorre à classe grafo e node implementada antes
    def solucaoBFS(self,start,goal):
        return self.g.procura_BFS(start,goal)
    
    def mostraA(self,e1,e2):
        discos_e1 = [e1[1], e1[3], e1[5], e1[7]]
        discos_e2 = [e2[1], e2[3], e2[5], e2[7]]

        for i in range(4):
            if discos_e1[i] != discos_e2[i]:
                return f"Mover disco D{i+1} de {discos_e1[i]} para {discos_e2[i]}"
    
    def imprimeA(self,caminho):
        lista_acoes=[]

        i=0
        while i+1 < len(caminho):
            lista_acoes.append(self.mostraA(caminho[i], caminho[i+1]))
            i = i + 1
        return lista_acoes

