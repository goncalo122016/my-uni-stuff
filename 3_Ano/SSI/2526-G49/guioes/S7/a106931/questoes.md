# Respostas às Questões — Semana 7

## Q1 — Padding acrescentado pelo length extension attack

### Mensagem e contexto
- Mensagem: `http://www.super-secret.com/manage?id=1001&role=user&perm=read` (63 bytes)
- Chave: 32 bytes (KEY_SIZE)
- Input total ao SHA256: `key || msg` = 32 + 63 = **95 bytes = 760 bits**

### Esquema de padding SHA256 (RFC 6234 / Merkle-Damgård)
1. Acrescentar byte `0x80` (bit '1' seguido de zeros)
2. Acrescentar bytes `0x00` até que o comprimento total ≡ 56 (mod 64) bytes
   (i.e., deixar espaço para 8 bytes de comprimento no último bloco de 64 bytes)
3. Acrescentar o comprimento original em bits como inteiro big-endian de 64 bits

### Cálculo
O URL `http://www.super-secret.com/manage?id=1001&role=user&perm=read` tem **62 bytes**.
- Input total: key (32) + msg (62) = **94 bytes = 752 bits**
- 94 = 64 + 30 → estamos 30 bytes dentro do bloco 2
- Após `0x80`: 31 bytes no bloco 2
- Precisamos de atingir 56 bytes (para deixar 8 para o length): 56 − 31 = **25 bytes `0x00`**
- Comprimento em bits: 752 = `0x00 0x00 0x00 0x00 0x00 0x00 0x02 0xF0` (8 bytes)

### Padding acrescentado
```
0x80  (1 byte)
0x00 × 25  (25 bytes de zeros)
0x00 0x00 0x00 0x00 0x00 0x00 0x02 0xF0  (8 bytes — comprimento 752 bits)
```
**Total: 34 bytes de padding** (verificado experimentalmente)

A mensagem estendida que o verificador autentica é:
```
http://www.super-secret.com/manage?id=1001&role=user&perm=read
|| <33 bytes de padding>
|| &admin=true
```

O hashpumpy calcula automaticamente este padding e forja um MAC válido para
esta mensagem alargada, **sem conhecer a chave**.

---

## Q2 — Diferença de tamanho entre pbenc_aes_ctr_hmac.py e pbenc_aes_gcm.py

### Estrutura dos ficheiros produzidos

| Campo        | AES-CTR + HMAC          | AES-GCM                  |
|--------------|--------------------------|---------------------------|
| salt         | 16 bytes                 | 16 bytes                  |
| nonce        | 16 bytes                 | 12 bytes                  |
| ciphertext   | N bytes (= plaintext)    | N bytes (= plaintext)     |
| tag de auth  | 32 bytes (HMAC-SHA256)   | 16 bytes (GCM tag)        |
| **Total**    | **N + 64 bytes**         | **N + 44 bytes**          |

### Justificação das diferenças

**Nonce:** AES-CTR usa 16 bytes (tamanho do bloco AES), AES-GCM usa 12 bytes
(96 bits — tamanho recomendado pelo NIST SP 800-38D para GCM, evitando o custo
de processar nonces de tamanho arbitrário com GHASH).

**Tag de autenticação:** O HMAC-SHA256 produz uma tag de 32 bytes (256 bits).
O GCM usa um autenticador GHASH de 128 bits (16 bytes). Logo o AES-GCM produz
ficheiros **20 bytes menores** do que AES-CTR+HMAC para a mesma mensagem.

**Ciphertext:** Ambos os modos usam AES-CTR internamente para cifrar, pelo que
o criptograma tem exactamente o mesmo tamanho do texto-limpo (sem padding) —
ao contrário do AES-CBC que acrescentaria até 16 bytes de padding PKCS7.

**Conclusão:** Para uma mensagem de N bytes:
- `pbenc_aes_ctr_hmac.py` → ficheiro de N + 64 bytes
- `pbenc_aes_gcm.py`      → ficheiro de N + 44 bytes

A diferença de 20 bytes deve-se ao nonce mais curto (−4 bytes) e à tag GCM
mais curta que o HMAC-SHA256 (−16 bytes). Em termos de segurança, a tag de
128 bits do GCM é suficiente para autenticação, enquanto os 256 bits do HMAC
oferecem margem adicional mas não são necessários na prática atual.
