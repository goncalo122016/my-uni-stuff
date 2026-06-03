import sys

def decode(str, chave):
    res = []
    for i in range(len(str)):
        c = str[i]
        k = chave[i % len(chave)]
        res.append(chr((ord(c) - ord(k) - 2 * ord('A')) % 26 + ord('A')))
    return res

def main():
    text = sys.argv[2]
    palavras = []
    for i in range(2, len(sys.argv)):
        if sys.argv[i].strip() == "":
            break
        palavras.append(sys.argv[i])

    freq = [0] * 26
    for i in range(len(text)):
        c = text[i]
        freq[ord(c) - ord('A')] += 1
    
    max_freq = 0
    for i in range(26):
        if freq[i] > freq[max_freq]:
            max_freq = i
    probable_keys = ['A', 'E', 'O', 'S', 'R', 'I', 'N', 'D', 'M', 'U', 'T', 'C', 'L', 'P', 'V', 'G', 'H', 'K', 'B', 'F', 'Z', 'J', 'X', 'Q', 'W', 'Y']  # Letras mais frequentes em português
    for i in range(26):
        chave = chr(ord('A') + max_freq)
        print(chave)
        msg = decode(text, chave)
        for p in palavras:
            if p in "".join(msg):
                print(chr(ord('A') + chave))
                print("".join(msg))
                break

if __name__ == "__main__":
    main()