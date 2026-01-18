class LojaFrutas:
    def __init__(self, nome, frutas):
        self.nome = nome
        self.frutas = frutas  # frutas é um dicionário com nome da fruta como chave e preço como valor
    
    def getCustoPorKg(self, fruta):
        if fruta in self.frutas:
            return self.frutas[fruta]
        else:
            print("Fruta não disponível")

    def getPrecoCompras(self,listaCompras):
        custoTotal = 0.0;
        for (fruta, numKgs) in listaCompras:
            if fruta in self.frutas:
                custoTotal += numKgs * self.frutas[fruta]
            else:
                print(f"Fruta {fruta} não disponível")
        return custoTotal

loja1 = LojaFrutas("Pomar de Zizu", {'maçã': 2.0, 'pera': 1.5, 'banana': 3.5})
print(loja1.getCustoPorKg('maçã'))
print(loja1.getPrecoCompras([('maçã', 2.0), ('banana', 1.0), ('laranja', 3.0)]))