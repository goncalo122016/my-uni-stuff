import pickle

print("Hello, World!")

print(type (5))

# Dicionários
meu_dicionario = dict()

meu_dicionario = {
    "nome": "João",
    "idade": 30,
    "cidade": "São Paulo"
}
meu_dicionario.update({"idade": 31})
print(meu_dicionario)

class Pessoa:
    def __init__(self, nome, idade):
        self.nome = nome
        self.idade = idade
    
    def print_info(self):
        print(f"Nome: {self.nome}, Idade: {self.idade}")
    
    def save_to_file(self, filename):
        with open(filename, 'wb') as file:
            pickle.dump(self, file)
    
    def load_from_file(filename):
        with open(filename, 'rb') as file:
            return pickle.load(file)
    
pessoa1 = Pessoa.load_from_file('Aula1/pessoa1.pkl')
pessoa1.print_info()
pessoa1.save_to_file('Aula1/pessoa1.pkl')

s = "Fim do programa"
print(s.split())

a = list()
b = set()
