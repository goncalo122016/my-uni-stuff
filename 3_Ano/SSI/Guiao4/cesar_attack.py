import sys

def decode(str, chave):
    res = []
    for c in str:
        res.append(chr((ord(c) - chave - ord('A')) % 26 + ord('A')))
    return res

def main():
    text = sys.argv[1]
    palavras = []
    for i in range(2, len(sys.argv)):
        if sys.argv[i].strip() == "":
            break
        palavras.append(sys.argv[i])

    for i in range(26):
        msg = decode(text, i)
        for p in palavras:
            if p in "".join(msg):
                print(chr(ord('A') + i))
                print("".join(msg))
                break

if __name__ == "__main__":
    main()