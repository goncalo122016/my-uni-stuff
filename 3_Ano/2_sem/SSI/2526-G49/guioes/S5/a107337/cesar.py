import sys

def encode(str, chave):
    res = []
    for c in str:
        res.append(chr((ord(c) + chave - ord('A')) % 26 + ord('A')))
    return res

def decode(str, chave):
    res = []
    for c in str:
        res.append(chr((ord(c) - chave - ord('A')) % 26 + ord('A')))
    return res

def preproc(str):
    l = []
    for c in str:
        if c.isalpha():
            l.append(c.upper())
    return "".join(l)

def main():
    if len(sys.argv) > 1:
        mode = sys.argv[1]
        chave = ord(sys.argv[2].upper()) - ord('A')
        text = preproc(sys.argv[3])
        # print("Modo: " + mode + ", Chave: " + str(chave) + ", Texto: " + text)
        if (mode == "enc"):
            msg = encode(text, chave)
            print("A mensagem codificada é: " + "".join(msg))
        elif (mode == "dec"):
            msg = decode(text, chave)
            print("A mensagem decodificada é: " + "".join(msg))
        else:
            print("Nenhum argumento foi fornecido.")

if __name__ == "__main__":
    main()
