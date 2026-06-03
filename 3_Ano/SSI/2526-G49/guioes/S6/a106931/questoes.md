# Respostas às Questões — Semana 6

## Q1 — Impacto de um NONCE fixo no ChaCha20

ChaCha20 é uma cifra sequencial síncrona: o criptograma é o XOR do texto-limpo
com um keystream determinado pela (chave, NONCE). Se o NONCE for fixo (ex: todos
zeros), o mesmo par (chave, NONCE) produz sempre o mesmo keystream.

**Consequência directa:** se dois ficheiros distintos forem cifrados com a mesma
chave e NONCE fixo, um atacante que observe ambos os criptogramas C1 e C2 pode
calcular:

    C1 XOR C2 = P1 XOR P2

Elimina-se o keystream e fica-se com o XOR dos dois textos-limpo. Com técnicas
de "crib dragging" (adivinhar palavras prováveis), é possível recuperar partes
significativas de ambas as mensagens. Este ataque é idêntico ao "two-time pad".

**Conclusão:** o NONCE deve ser sempre aleatório e único por mensagem (nunca
reutilizado com a mesma chave), mesmo que isso implique guardá-lo junto ao
criptograma.

---

## Q2 — Difusão em ChaCha20: 1 bit no plaintext afecta quantos no criptograma?

**Apenas 1 bit.**

ChaCha20 cifra por XOR bit-a-bit com o keystream:
    ciphertext[i] = plaintext[i] XOR keystream[i]

Alterar 1 bit de plaintext[i] altera exactamente 1 bit de ciphertext[i] — os
restantes bytes do criptograma ficam inalterados. Não há difusão (propriedade
exclusiva de cifras de bloco). Isto confirma que ChaCha20 não garante integridade:
modificações cirúrgicas no criptograma produzem modificações previsíveis no
texto-limpo decifrado.

---

## Q3 — Bit flips em AES-CBC e AES-CTR

### AES-CTR
CTR converte AES numa cifra sequencial: ciphertext[i] = plaintext[i] XOR keystream[i].
Tal como no ChaCha20, flipar **1 bit** no criptograma afecta exactamente **1 bit**
no plaintext decifrado — sem qualquer propagação a outros bytes.

### AES-CBC
No modo CBC, a decifração de cada bloco depende do bloco cifrado anterior:
    P[k] = AES_dec(C[k]) XOR C[k-1]

Se fliparmos 1 bit no bloco C[k] do criptograma:
1. **Bloco k do plaintext** — o AES_dec(C[k]) produz saída completamente aleatória
   (todos os 128 bits são afectados). Perde-se o bloco inteiro.
2. **Bloco k+1 do plaintext** — o XOR com C[k] propaga exactamente **1 bit** flipado
   para P[k+1] (o bit correspondente ao que foi alterado).
3. Blocos k+2, k+3, ... **não são afectados**.

**Resumo:**
| Modo | Bits afectados no plaintext |
|------|-----------------------------|
| CTR  | 1 bit (posição exacta)      |
| CBC  | 128 bits (bloco k) + 1 bit (bloco k+1) |

Nota: flipar um bit no IV (primeiros 16 bytes) de CBC afecta apenas 1 bit no
primeiro bloco do plaintext.

---

## Q4 — Impacto de chacha20_int_attck.py sobre criptogramas AES-CBC e AES-CTR

O ataque calcula:
    ctxt[NONCE_OFFSET + pos + i] ^= ptxt[i] ^ new_ptxt[i]

### AES-CTR
CTR é também uma cifra sequencial (XOR com keystream), com estrutura idêntica ao
ChaCha20. O ataque funciona **igualmente bem**: flipar bits no criptograma em
posição `pos` (após os 16 bytes de nonce/IV) produz exactamente a substituição
`ptxtAtPos` → `newPtxtAtPos` no plaintext decifrado. **O ataque é eficaz.**

### AES-CBC
O ataque **não produz o efeito desejado**. Como visto na Q3:
- Alterar bytes em C[k] corrompe completamente o bloco k do plaintext (128 bits
  aleatórios), impossibilitando a substituição precisa pretendida.
- O flip "vazará" para o bloco seguinte (k+1) de forma previsível, mas isso não
  corresponde ao objectivo do ataque.

**Conclusão:** `chacha20_int_attck.py` é eficaz em cifras sequenciais (ChaCha20,
AES-CTR), mas ineficaz em AES-CBC (a não ser que se ataque o IV ou se queira
corromper um bloco inteiro aceitando a perda do bloco anterior).

---

## Q5 — Função do salt e do NONCE no pbenc_chacha20.py

**Salt (16 bytes, guardado no ficheiro):**
- Alimenta o PBKDF2 para derivar a chave a partir da passphrase.
- Garante que a mesma passphrase produz chaves diferentes em cifras distintas,
  impedindo ataques de dicionário pré-computados (rainbow tables).
- Sem salt, dois utilizadores com a mesma passphrase teriam a mesma chave — um
  atacante que quebre uma instância quebra todas as outras.

**NONCE (16 bytes, guardado no ficheiro):**
- Usado directamente pelo ChaCha20 para gerar o keystream.
- Garante que, mesmo que salt e passphrase sejam iguais (logo a chave é igual),
  o keystream é diferente — evitando reutilização de keystream (two-time pad).

**São ambos necessários conjuntamente?**
Tecnicamente, se o salt for sempre aleatório e único, a chave derivada é sempre
diferente, tornando o NONCE redundante (a chave única já impede a reutilização
do keystream). No entanto, **ambos devem ser usados**: o salt protege contra
ataques à passphrase (pré-computação), o NONCE protege contra reutilização do
keystream na cifra — constituem defesas em camadas contra ameaças distintas.
Na prática, um erro de implementação que reutilize o salt torna o NONCE
a última linha de defesa, e vice-versa.
