# Respostas às Questões — Semana 8

## Q1 — A aplicação garante PFS (Perfect Forward Secrecy)?

**Sim, garante PFS**, desde que as chaves DH efémeras sejam descartadas após
cada sessão (o que acontece naturalmente porque são geradas dentro do processo).

### Justificação

No protocolo DH implementado, Alice e Bob geram pares de chaves *efémeros*
(x, g^x) e (y, g^y) **novos em cada execução**. O segredo partilhado K = g^(xy)
é único por sessão. Após a sessão, x e y são destruídos (saem do âmbito).

PFS significa: mesmo que o segredo K de uma sessão seja comprometido no futuro,
as comunicações de *outras* sessões (passadas ou futuras) permanecem seguras.

- **Sessão comprometida**: se um atacante obtiver K de uma sessão, consegue
  derivar a mesma chave AES-GCM via HKDF e decifrar as mensagens dessa sessão.
  Mas K é específico de (x, y) efémeros que já foram descartados.
- **Outras sessões**: usam x', y' independentes → K' = g^(x'y') completamente
  diferente. Comprometer K não dá qualquer informação sobre K'.
- **HKDF**: é uma função determinística de K; não acrescenta nem remove PFS.
  Se K é único por sessão, a chave derivada também o é.

**Conclusão**: a utilização de chaves DH efémeras (EDH — Ephemeral DH) é
exactamente o mecanismo que garante PFS. O protocolo implementado é seguro
neste aspecto.

---

## Q2 — Onde estão armazenadas as chaves públicas de cada participante?

As chaves públicas estão **dentro dos certificados X.509** (ficheiros `.crt`).

Um certificado X.509 é uma estrutura ASN.1 que contém, entre outros campos:
- **Subject** (nome do titular, e.g. "CN=Alice")
- **SubjectPublicKeyInfo** — a chave pública RSA do titular
- **Issuer** (nome de quem assinou, e.g. "CN=CA")
- **Validity** (período de validade)
- **Signature** — assinatura digital da CA sobre todos os campos acima

Ao enviar o certificado ao outro participante (e.g. CertB de Bob para Alice),
está-se a enviar simultaneamente a chave pública de Bob E a prova de que essa
chave foi certificada pela CA. A CA garante a autenticidade da ligação entre
a identidade ("CN=Bob") e a chave pública.

Pode-se inspecionar o conteúdo de um certificado com:
```
openssl x509 -in Bob.crt -text -noout
```

---

## Q3 — STS sem verificação do certificado do Bob é imune a MitM?

**Não. O protocolo deixa de ser imune a MitM.**

### Ataque possível

Se Alice não verificar o CertB contra a CA, aceita *qualquer* certificado que
Bob (ou um atacante) lhe envie. Um atacante Mallory pode:

1. Interceptar a mensagem de Alice com g^x.
2. Fazer o seu próprio DH com Alice: enviar g^m (chave efémera de Mallory),
   juntamente com um certificado **auto-assinado** `CertM` com *qualquer* nome.
3. Alice verifica `SigM(g^m, g^x)` com a chave pública de `CertM` — a verificação
   **passa** porque a assinatura é genuína para aquele certificado.
4. Alice acredita estar a falar com Bob, mas partilhou K' = g^(xm) com Mallory.
5. Mallory faz DH separado com Bob, estabelece K'' = g^(my).
6. Mallory lê e re-cifra todas as mensagens entre Alice e Bob.

### Por que a verificação da assinatura não chega?

A assinatura apenas prova que *quem tem a chave privada correspondente ao
certificado enviado* conhece g^y e g^x. Sem verificar que o certificado foi
emitido por uma CA de confiança, Alice não sabe *a quem* pertence essa chave.
Qualquer atacante pode gerar o seu próprio par RSA e auto-assinar um certificado
em nome de "Bob". A verificação da assinatura passaria, mas Alice estaria a
comunicar com o atacante, não com o Bob legítimo.

A verificação do certificado pela CA é **essencial** para ligar a identidade
("CN=Bob") à chave pública de forma verificável — é esse o propósito da PKI.
