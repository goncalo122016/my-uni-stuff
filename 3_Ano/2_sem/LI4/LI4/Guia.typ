= BELAVISTA --- Guia de Instalação, Operação e Manutenção
<belavista-guia-de-instalação-operação-e-manutenção>
#quote(block: true)[
Sistema de Gestão de Cadeia de Lojas de Conveniência \
LI4 · Grupo 9 · Universidade do Minho · 2025/2026
]

#horizontalrule

== Índice
<índice>
+ #link(<1-visão-geral-do-sistema>)[Visão Geral do Sistema]
+ #link(<2-requisitos-de-sistema>)[Requisitos de Sistema]
+ #link(<3-instalação>)[Instalação]
  - 3.1 #link(<31-clonar-o-repositório>)[Clonar o Repositório]
  - 3.2 #link(<32-compilar-os-backends>)[Compilar os Backends]
  - 3.3
    #link(<33-instalar-dependências-do-frontend>)[Instalar Dependências do Frontend]
+ #link(<4-configuração>)[Configuração]
  - 4.1 #link(<41-backend-cadeia>)[backend-cadeia]
  - 4.2 #link(<42-backend-loja>)[backend-loja]
  - 4.3 #link(<43-frontend>)[Frontend]
+ #link(<5-iniciar-o-sistema>)[Iniciar o Sistema]
+ #link(<6-credenciais-de-acesso>)[Credenciais de Acesso]
+ #link(<7-guia-de-operação>)[Guia de Operação]
  - 7.1 #link(<71-interface-web-frontend>)[Interface Web (Frontend)]
  - 7.2 #link(<72-perfis-de-utilizador>)[Perfis de Utilizador]
  - 7.3 #link(<73-fluxos-principais>)[Fluxos Principais]
+ #link(<8-api-rest--referência-rápida>)[API REST --- Referência Rápida]
  - 8.1
    #link(<81-backend-cadeia-porta-8080>)[backend-cadeia (porta 8080)]
  - 8.2 #link(<82-backend-loja-porta-8081>)[backend-loja (porta 8081)]
+ #link(<9-sincronização-cadeia--loja>)[Sincronização Cadeia ↔ Loja]
+ #link(<10-base-de-dados>)[Base de Dados]
+ #link(<11-manutenção>)[Manutenção]
  - 11.1 #link(<111-logs>)[Logs]
  - 11.2 #link(<112-backup>)[Backup]
  - 11.3 #link(<113-reinício-dos-serviços>)[Reinício dos Serviços]
  - 11.4 #link(<114-atualização-do-sistema>)[Atualização do Sistema]
+ #link(<12-resolução-de-problemas-troubleshooting>)[Resolução de Problemas (Troubleshooting)]
+ #link(<13-paragem-do-sistema>)[Paragem do Sistema]

#horizontalrule

== 1. Visão Geral do Sistema
<visão-geral-do-sistema>
O #strong[BELAVISTA] é um sistema distribuído de gestão para uma cadeia
de lojas de conveniência, composto por três componentes independentes
que comunicam entre si:

```
┌─────────────────────────────────────────────────────────┐
│                     BELAVISTA                           │
│                                                         │
│  ┌─────────────────┐      ┌──────────────────────────┐  │
│  │   Frontend       │      │    backend-cadeia        │  │
│  │   (Next.js)      │◄────►│    (Central da Cadeia)   │  │
│  │   porta 3000     │      │    porta 8080            │  │
│  └─────────────────┘      └──────────┬───────────────┘  │
│           │                          │ sync diária       │
│           │                          ▼                   │
│           │               ┌──────────────────────────┐  │
│           └──────────────►│    backend-loja           │  │
│                           │    (Loja Local)           │  │
│                           │    porta 8081            │  │
│                           └──────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

#figure(
  align(center)[#table(
    columns: (25%, 25%, 25%, 25%),
    align: (auto,auto,auto,auto,),
    table.header([Componente], [Tecnologia], [Porta], [Responsabilidade],),
    table.hline(),
    [#strong[backend-cadeia];], [Spring Boot 3.4 +
    Kotlin], [8080], [Gestão central: lojas, fornecedores, produtos do
    catálogo, funcionários, relatórios consolidados],
    [#strong[backend-loja];], [Spring Boot 3.4 +
    Kotlin], [8081], [Gestão local: vendas, stock, clientes, devoluções,
    faturas],
    [#strong[frontend];], [Next.js 16 + TypeScript], [3000], [Interface
    web para todos os utilizadores],
  )]
  , kind: table
  )

#strong[Base de dados:] SQLite (ficheiros `cadeia.db` e `loja.db` na
raiz de cada backend).

#horizontalrule

== 2. Requisitos de Sistema
<requisitos-de-sistema>
=== Software obrigatório
<software-obrigatório>
#figure(
  align(center)[#table(
    columns: 3,
    align: (auto,auto,auto,),
    table.header([Software], [Versão mínima], [Verificação],),
    table.hline(),
    [#strong[Java (JDK)];], [21], [`java -version`],
    [#strong[Node.js];], [18 LTS], [`node -v`],
    [#strong[npm];], [9], [`npm -v`],
  )]
  , kind: table
  )

=== Software opcional (para desenvolvimento/rebuild)
<software-opcional-para-desenvolvimentorebuild>
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    table.header([Software], [Uso],),
    table.hline(),
    [#strong[Gradle];], [Recompilar os backends (o projeto inclui
    `gradlew`)],
    [#strong[Git];], [Clonar e atualizar o repositório],
  )]
  , kind: table
  )

=== Recursos de hardware (mínimos)
<recursos-de-hardware-mínimos>
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    table.header([Recurso], [Mínimo recomendado],),
    table.hline(),
    [RAM], [1 GB disponível],
    [Disco], [500 MB],
    [CPU], [Dual-core],
    [Rede], [Comunicação local entre as três portas],
  )]
  , kind: table
  )

=== Sistema operativo
<sistema-operativo>
Compatível com #strong[Linux];, #strong[macOS] e #strong[Windows] (com
WSL ou PowerShell).

#horizontalrule

== 3. Instalação
<instalação>
=== 3.1 Clonar o Repositório
<clonar-o-repositório>
```bash
git clone <url-do-repositorio>
cd LI4
```

=== 3.2 Compilar os Backends
<compilar-os-backends>
#quote(block: true)[
#strong[Nota:] Os ficheiros JAR já compilados encontram-se em \
`backend-cadeia/build/libs/` e `backend-loja/build/libs/`. \
Se não for necessário recompilar, avance para a secção 5.
]

Para recompilar (requer Java 21):

```bash
# Na raiz do projeto
./gradlew :backend-cadeia:bootJar :backend-loja:bootJar
```

No Windows (sem WSL):

```cmd
gradlew.bat :backend-cadeia:bootJar :backend-loja:bootJar
```

Os JARs são gerados em: -
`backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar` -
`backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar`

=== 3.3 Instalar Dependências do Frontend
<instalar-dependências-do-frontend>
```bash
cd frontend
npm install
```

#horizontalrule

== 4. Configuração
<configuração>
=== 4.1 backend-cadeia
<backend-cadeia>
Ficheiro: `backend-cadeia/src/main/resources/application.yml`

```yaml
spring:
  datasource:
    url: jdbc:sqlite:./cadeia.db        # caminho para a base de dados
  jpa:
    hibernate:
      ddl-auto: create                  # "create" reinicia o schema; usar "update" em produção

server:
  port: 8080                            # porta do backend-cadeia

loja:
  url: http://localhost:8081            # URL do backend-loja
```

#quote(block: true)[
#strong[Importante:] `ddl-auto: create` apaga e recria as tabelas a cada
arranque. \
Em produção ou para preservar dados, alterar para `ddl-auto: update`.
]

=== 4.2 backend-loja
<backend-loja>
Ficheiro: `backend-loja/src/main/resources/application.yml`

```yaml
spring:
  datasource:
    url: jdbc:sqlite:./loja.db          # caminho para a base de dados
  jpa:
    hibernate:
      ddl-auto: create                  # alterar para "update" em produção

server:
  port: 8081                            # porta do backend-loja

loja:
  id: 1                                 # identificador único desta loja

cadeia:
  url: http://localhost:8080            # URL do backend-cadeia
```

#quote(block: true)[
Para instalar o sistema em #strong[múltiplas lojas físicas];, cada
instância do `backend-loja` deve ter um `loja.id` diferente e apontar
`cadeia.url` para o servidor central correto.
]

=== 4.3 Frontend
<frontend>
Ficheiro: `frontend/next.config.ts`

```typescript
// As chamadas ao frontend são reencaminhadas para os backends:
"/loja-api/*"    →  http://localhost:8081/api/*   (backend-loja)
"/cadeia-api/*"  →  http://localhost:8080/api/*   (backend-cadeia)
```

Se os backends estiverem em servidores diferentes, alterar os valores de
`destination` neste ficheiro.

#horizontalrule

== 5. Iniciar o Sistema
<iniciar-o-sistema>
Os três componentes devem ser iniciados #strong[pela seguinte ordem];:

=== Passo 1 --- Iniciar o backend-cadeia
<passo-1-iniciar-o-backend-cadeia>
```bash
java -Xmx256m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar
```

Aguardar até aparecer no terminal:

```
Started CadeiaApplication in X.XXX seconds
```

=== Passo 2 --- Iniciar o backend-loja
<passo-2-iniciar-o-backend-loja>
Abrir um #strong[novo terminal] e executar:

```bash
java -Xmx256m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar
```

Aguardar até aparecer:

```
Started LojaApplication in X.XXX seconds
```

=== Passo 3 --- Iniciar o Frontend
<passo-3-iniciar-o-frontend>
Abrir um #strong[terceiro terminal];:

```bash
cd frontend

# Modo produção (recomendado):
npm run build
npm run start

# Modo desenvolvimento:
npm run dev
```

=== Verificação
<verificação>
Após iniciar todos os componentes, aceder a:

#figure(
  align(center)[#table(
    columns: (33.33%, 33.33%, 33.33%),
    align: (auto,auto,auto,),
    table.header([URL], [Componente], [Esperado],),
    table.hline(),
    [http:\/\/localhost:3000], [Frontend], [Página de login BELAVISTA],
    [http:\/\/localhost:8080/swagger-ui.html], [API
    Cadeia], [Documentação Swagger],
    [http:\/\/localhost:8081/swagger-ui.html], [API Loja], [Documentação
    Swagger],
  )]
  , kind: table
  )

#horizontalrule

== 6. Credenciais de Acesso
<credenciais-de-acesso>
=== backend-cadeia (Sede da Cadeia)
<backend-cadeia-sede-da-cadeia>
#figure(
  align(center)[#table(
    columns: 3,
    align: (auto,auto,auto,),
    table.header([Número], [Senha], [Perfil],),
    table.hline(),
    [`ADM001`], [`admin123`], [Administrador],
    [`GER001`], [`gerente123`], [Gestor],
  )]
  , kind: table
  )

=== backend-loja (Loja Local)
<backend-loja-loja-local>
#figure(
  align(center)[#table(
    columns: 3,
    align: (auto,auto,auto,),
    table.header([Número], [Senha], [Perfil],),
    table.hline(),
    [`ADM001`], [`admin123`], [Administrador],
    [`GER001`], [`gerente123`], [Gestor],
    [`FUN001`], [`func123`], [Funcionário],
    [`FUN002`], [`func123`], [Funcionário],
  )]
  , kind: table
  )

#quote(block: true)[
#strong[Segurança:] As senhas estão armazenadas em texto simples na base
de dados. \
Em ambiente de produção real, devem ser substituídas por hashes (ex.:
bcrypt).
]

#horizontalrule

== 7. Guia de Operação
<guia-de-operação>
=== 7.1 Interface Web (Frontend)
<interface-web-frontend>
Aceder a #strong[http:\/\/localhost:3000] num browser moderno (Chrome,
Firefox, Edge).

O sistema deteta automaticamente se o utilizador pertence à
#strong[cadeia] ou à #strong[loja] e apresenta o dashboard
correspondente.

=== 7.2 Perfis de Utilizador
<perfis-de-utilizador>
==== Administrador da Cadeia (`ADM`)
<administrador-da-cadeia-adm>
- Gerir lojas da cadeia (criar, consultar)
- Gerir funcionários
- Gerir fornecedores
- Gerir catálogo de produtos central
- Consultar estatísticas consolidadas
- Gerar relatórios da cadeia
- Consultar sincronizações

==== Gestor da Cadeia (`GER`)
<gestor-da-cadeia-ger>
- Consultar estatísticas e relatórios
- Consultar lojas e fornecedores

==== Administrador da Loja (`ADM`)
<administrador-da-loja-adm>
- Gerir stock local (entradas, ajustes)
- Gerir funcionários da loja
- Gerir clientes
- Consultar vendas e histórico
- Processar devoluções
- Forçar sincronização com a cadeia

==== Gestor da Loja (`GER`)
<gestor-da-loja-ger>
- Consultar stock e produtos
- Consultar vendas e histórico

==== Funcionário da Loja (`FUN`)
<funcionário-da-loja-fun>
- Registar vendas
- Emitir faturas
- Registar devoluções
- Consultar produtos e stock

=== 7.3 Fluxos Principais
<fluxos-principais>
==== Registar uma Venda
<registar-uma-venda>
+ Autenticar como Funcionário ou Gestor da Loja
+ Navegar para #strong[Vendas → Nova Venda]
+ Selecionar cliente (ou venda anónima)
+ Adicionar linhas de venda (produto + quantidade)
+ Confirmar a venda
+ Emitir fatura se necessário

==== Gerir Stock
<gerir-stock>
+ Autenticar como Administrador da Loja
+ Navegar para #strong[Stock]
+ Para entrada de mercadoria: #strong[Entrada de Stock] → produto +
  quantidade
+ Para ajuste de inventário: #strong[Ajuste de Stock] → produto + nova
  quantidade

==== Registar uma Devolução
<registar-uma-devolução>
+ Autenticar como Funcionário ou superior
+ Navegar para #strong[Vendas] → localizar a venda
+ Selecionar #strong[Registar Devolução]
+ Indicar os itens a devolver

==== Consultar Relatórios Consolidados (Cadeia)
<consultar-relatórios-consolidados-cadeia>
+ Autenticar na cadeia como ADM ou GER
+ Navegar para #strong[Central → Relatórios]
+ Criar novo relatório ou consultar os existentes
+ Consultar estatísticas por loja

==== Adicionar uma Nova Loja à Cadeia
<adicionar-uma-nova-loja-à-cadeia>
+ Autenticar na cadeia como ADM
+ Navegar para #strong[Cadeia → Lojas → Nova Loja]
+ Preencher nome e localização
+ Configurar uma nova instância do `backend-loja` com o `loja.id`
  correspondente

#horizontalrule

== 8. API REST --- Referência Rápida
<api-rest-referência-rápida>
A documentação interativa completa está disponível via Swagger: -
Cadeia: http:\/\/localhost:8080/swagger-ui.html - Loja:
http:\/\/localhost:8081/swagger-ui.html

=== 8.1 backend-cadeia (porta 8080)
<backend-cadeia-porta-8080>
#figure(
  align(center)[#table(
    columns: (33.33%, 33.33%, 33.33%),
    align: (auto,auto,auto,),
    table.header([Método], [Endpoint], [Descrição],),
    table.hline(),
    [`POST`], [`/api/auth/login`], [Autenticação],
    [`GET`], [`/api/cadeia`], [Obter informação da cadeia],
    [`POST`], [`/api/cadeia`], [Criar cadeia],
    [`POST`], [`/api/cadeia/{id}/lojas`], [Associar loja à cadeia],
    [`GET`], [`/api/lojas`], [Listar todas as lojas],
    [`GET`], [`/api/lojas/{id}`], [Obter loja por ID],
    [`GET`], [`/api/lojas/{id}/estatisticas`], [Estatísticas de uma
    loja],
    [`POST`], [`/api/lojas/{id}/fornecedores`], [Associar fornecedor a
    loja],
    [`POST`], [`/api/lojas/{id}/sync`], [Forçar sync de uma loja],
    [`GET`], [`/api/fornecedores`], [Listar fornecedores],
    [`GET`], [`/api/fornecedores/{id}`], [Obter fornecedor],
    [`POST`], [`/api/fornecedores`], [Criar fornecedor],
    [`GET`], [`/api/produtos`], [Listar produtos do catálogo],
    [`GET`], [`/api/produtos/{id}`], [Obter produto],
    [`POST`], [`/api/produtos`], [Criar produto],
    [`PUT`], [`/api/produtos/{id}`], [Atualizar produto],
    [`GET`], [`/api/utilizadores`], [Listar funcionários],
    [`GET`], [`/api/utilizadores/{id}`], [Obter funcionário],
    [`POST`], [`/api/utilizadores`], [Criar funcionário],
    [`PUT`], [`/api/utilizadores/{id}`], [Atualizar funcionário],
    [`PATCH`], [`/api/utilizadores/{id}/loja`], [Atribuir loja ao
    funcionário],
    [`GET`], [`/api/central/estatisticas`], [Estatísticas consolidadas],
    [`POST`], [`/api/central/relatorios`], [Gerar relatório],
    [`GET`], [`/api/central/relatorios`], [Listar relatórios],
    [`GET`], [`/api/central/sincronizacoes`], [Listar sincronizações],
    [`POST`], [`/api/sync/importar`], [Receber dados de sincronização
    #emph[(chamado pela loja)];],
  )]
  , kind: table
  )

=== 8.2 backend-loja (porta 8081)
<backend-loja-porta-8081>
#figure(
  align(center)[#table(
    columns: 3,
    align: (auto,auto,auto,),
    table.header([Método], [Endpoint], [Descrição],),
    table.hline(),
    [`POST`], [`/api/auth/login`], [Autenticação],
    [`GET`], [`/api/produtos`], [Listar produtos locais],
    [`GET`], [`/api/produtos/{id}`], [Obter produto],
    [`GET`], [`/api/stock`], [Listar stock],
    [`GET`], [`/api/stock/produto/{produtoId}`], [Stock de um produto],
    [`POST`], [`/api/stock/entrada`], [Registar entrada de stock],
    [`POST`], [`/api/stock/ajuste`], [Ajustar stock],
    [`GET`], [`/api/clientes`], [Listar clientes],
    [`GET`], [`/api/clientes/{id}`], [Obter cliente],
    [`POST`], [`/api/clientes`], [Criar cliente],
    [`PUT`], [`/api/clientes/{id}`], [Atualizar cliente],
    [`GET`], [`/api/utilizadores`], [Listar funcionários],
    [`GET`], [`/api/utilizadores/{id}`], [Obter funcionário],
    [`POST`], [`/api/utilizadores`], [Criar funcionário],
    [`PUT`], [`/api/utilizadores/{id}`], [Atualizar funcionário],
    [`POST`], [`/api/vendas`], [Registar venda],
    [`POST`], [`/api/vendas/{id}/fatura`], [Emitir fatura],
    [`POST`], [`/api/vendas/{id}/devolucao`], [Registar devolução],
    [`GET`], [`/api/vendas/historico`], [Histórico de vendas],
    [`POST`], [`/api/vendas/sync`], [Forçar sincronização com cadeia],
  )]
  , kind: table
  )

#horizontalrule

== 9. Sincronização Cadeia ↔ Loja
<sincronização-cadeia-loja>
O `backend-loja` envia automaticamente um resumo das vendas do dia para
o `backend-cadeia`.

=== Sincronização Automática
<sincronização-automática>
- #strong[Horário:] Todos os dias às #strong[23:55]
- #strong[Dados enviados:] total de vendas, número de transações,
  produtos vendidos

=== Sincronização Manual
<sincronização-manual>
Via API:

```bash
POST http://localhost:8081/api/vendas/sync
```

Via interface web: - Autenticar como ADM na loja → #strong[Sync com
Cadeia]

=== O que é sincronizado
<o-que-é-sincronizado>
- Totais financeiros das vendas do dia
- Número de transações
- Os dados detalhados (linhas de venda, clientes) #strong[não] são
  enviados --- ficam apenas na loja

#horizontalrule

== 10. Base de Dados
<base-de-dados>
=== Localização dos ficheiros
<localização-dos-ficheiros>
#figure(
  align(center)[#table(
    columns: 3,
    align: (auto,auto,auto,),
    table.header([Ficheiro], [Localização], [Conteúdo],),
    table.hline(),
    [`cadeia.db`], [`backend-cadeia/cadeia.db`], [Dados centrais da
    cadeia],
    [`loja.db`], [`backend-loja/loja.db`], [Dados locais da loja],
  )]
  , kind: table
  )

=== Tabelas principais --- cadeia.db
<tabelas-principais-cadeia.db>
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    table.header([Tabela], [Descrição],),
    table.hline(),
    [`cadeia`], [Informação da cadeia],
    [`loja`], [Lojas registadas],
    [`produto`], [Catálogo de produtos central],
    [`categoria`], [Categorias de produtos],
    [`fornecedor`], [Fornecedores],
    [`funcionario`], [Funcionários da cadeia],
    [`relatorio`], [Relatórios gerados],
    [`sincronizacao_loja`], [Registos de sincronização recebidos],
  )]
  , kind: table
  )

=== Tabelas principais --- loja.db
<tabelas-principais-loja.db>
#figure(
  align(center)[#table(
    columns: 2,
    align: (auto,auto,),
    table.header([Tabela], [Descrição],),
    table.hline(),
    [`produto`], [Produtos disponíveis na loja],
    [`stock`], [Níveis de stock por produto],
    [`cliente`], [Clientes registados],
    [`funcionario`], [Funcionários da loja],
    [`venda`], [Vendas realizadas],
    [`linha_de_venda`], [Linhas de cada venda],
    [`fatura`], [Faturas emitidas],
    [`devolucao`], [Devoluções registadas],
    [`historico_de_vendas`], [Histórico consolidado],
  )]
  , kind: table
  )

=== Acesso direto à base de dados (SQLite)
<acesso-direto-à-base-de-dados-sqlite>
```bash
# Instalar o cliente SQLite
# Ubuntu/Debian: sudo apt install sqlite3
# macOS: brew install sqlite

# Consultar a base de dados da cadeia
sqlite3 backend-cadeia/cadeia.db

# Exemplos de queries
.tables                          -- listar tabelas
SELECT * FROM funcionario;       -- ver todos os funcionários
SELECT * FROM sincronizacao_loja ORDER BY data DESC LIMIT 10;  -- últimas sync
.quit
```

#horizontalrule

== 11. Manutenção
<manutenção>
=== 11.1 Logs
<logs>
Os logs são apresentados no terminal onde cada processo foi iniciado.

#strong[Nível de log por defeito:] `DEBUG` para `com.li4.*`

Para reduzir a verbosidade em produção, alterar no `application.yml`:

```yaml
logging:
  level:
    com.li4: INFO
```

Para guardar logs em ficheiro:

```bash
# Redirecionar output para ficheiro
java -Xmx256m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar \
  > logs/cadeia-$(date +%Y%m%d).log 2>&1 &

java -Xmx256m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar \
  > logs/loja-$(date +%Y%m%d).log 2>&1 &
```

=== 11.2 Backup
<backup>
#strong[Backup manual das bases de dados:]

```bash
# Criar diretório de backups
mkdir -p backups

# Backup da cadeia
cp backend-cadeia/cadeia.db backups/cadeia-$(date +%Y%m%d-%H%M%S).db

# Backup da loja
cp backend-loja/loja.db backups/loja-$(date +%Y%m%d-%H%M%S).db
```

#quote(block: true)[
Fazer o backup com os backends #strong[parados] ou usando o comando
SQLite `.backup`:

```bash
sqlite3 backend-cadeia/cadeia.db ".backup backups/cadeia-backup.db"
```
]

#strong[Recomendação:] Agendar backups diários (ex.: via `cron` no
Linux):

```bash
# Editar crontab: crontab -e
# Backup todos os dias às 02:00
0 2 * * * cp /caminho/para/LI4/backend-cadeia/cadeia.db /backups/cadeia-$(date +\%Y\%m\%d).db
0 2 * * * cp /caminho/para/LI4/backend-loja/loja.db /backups/loja-$(date +\%Y\%m\%d).db
```

=== 11.3 Reinício dos Serviços
<reinício-dos-serviços>
#strong[Parar os serviços:]

```bash
# Encontrar os PIDs
ps aux | grep "backend-cadeia\|backend-loja"

# Terminar os processos
kill <PID-cadeia>
kill <PID-loja>
```

#strong[Reiniciar:]

```bash
# Reiniciar pela ordem correta: cadeia → loja → frontend
java -Xmx256m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar &
sleep 10
java -Xmx256m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar &
cd frontend && npm run start &
```

=== 11.4 Atualização do Sistema
<atualização-do-sistema>
+ #strong[Parar todos os serviços]

+ #strong[Fazer backup das bases de dados] (ver 11.2)

+ #strong[Atualizar o código:]

  ```bash
  git pull origin main
  ```

+ #strong[Recompilar os backends:]

  ```bash
  ./gradlew :backend-cadeia:bootJar :backend-loja:bootJar
  ```

+ #strong[Atualizar dependências do frontend:]

  ```bash
  cd frontend && npm install
  npm run build
  ```

+ #strong[Alterar `ddl-auto` para `update`] nos ficheiros
  `application.yml` de ambos os backends (para preservar dados
  existentes)

+ #strong[Reiniciar os serviços] (ver 11.3)

#horizontalrule

== 12. Resolução de Problemas (Troubleshooting)
<resolução-de-problemas-troubleshooting>
=== O backend não inicia --- `Port 8080 already in use`
<o-backend-não-inicia-port-8080-already-in-use>
```bash
# Identificar o processo que usa a porta
lsof -i :8080     # Linux/macOS
netstat -ano | findstr :8080   # Windows

# Terminar o processo
kill -9 <PID>
```

=== Erro de ligação entre loja e cadeia
<erro-de-ligação-entre-loja-e-cadeia>
Verificar: 1. O `backend-cadeia` está em execução
(http:\/\/localhost:8080/swagger-ui.html acessível) 2. `cadeia.url` em
`backend-loja/application.yml` aponta para o endereço correto 3.
Firewall não bloqueia a porta 8080

=== Base de dados apagada a cada reinício
<base-de-dados-apagada-a-cada-reinício>
O `ddl-auto: create` #strong[apaga e recria] o schema a cada arranque.
Para preservar dados:

```yaml
# Em application.yml (ambos os backends)
jpa:
  hibernate:
    ddl-auto: update   # ← alterar de "create" para "update"
```

=== Frontend não consegue comunicar com os backends
<frontend-não-consegue-comunicar-com-os-backends>
Verificar `frontend/next.config.ts`:

```typescript
destination: "http://localhost:8081/api/:path*"  // IP/porta corretos?
destination: "http://localhost:8080/api/:path*"  // IP/porta corretos?
```

Após alterar o ficheiro, recompilar o frontend:
`npm run build && npm run start`

=== Erro de autenticação --- `401 Unauthorized`
<erro-de-autenticação-401-unauthorized>
- Verificar que o número e senha estão corretos (ver secção 6)
- Confirmar que o backend correto está a ser usado (cadeia vs.~loja)
- Os dados são reiniciados se `ddl-auto: create` estiver activo --- as
  credenciais por defeito são restauradas

=== Sincronização falha
<sincronização-falha>
+ Verificar que ambos os backends estão em execução
+ Verificar os logs do `backend-loja` para mensagens de erro
+ Forçar sync manual: `POST http://localhost:8081/api/vendas/sync`
+ Verificar em `backend-cadeia`:
  `GET http://localhost:8080/api/central/sincronizacoes`

=== Memória insuficiente ao correr ambos os backends
<memória-insuficiente-ao-correr-ambos-os-backends>
Cada backend é iniciado com `-Xmx256m` (256 MB máximo). Para reduzir o
consumo:

```bash
java -Xmx128m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar
java -Xmx128m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar
```

#horizontalrule

== 13. Paragem do Sistema
<paragem-do-sistema>
Para parar o sistema de forma limpa:

```bash
# 1. Parar o frontend (Ctrl+C no terminal, ou:)
kill $(lsof -ti :3000)

# 2. Parar o backend-loja (aguardar que a sync pendente termine)
kill $(lsof -ti :8081)

# 3. Parar o backend-cadeia
kill $(lsof -ti :8080)
```

#quote(block: true)[
A paragem do `backend-loja` antes das 23:55 num dia com vendas
#strong[não] perde dados --- \
as vendas ficam guardadas na base de dados local e a sync pode ser
forçada manualmente \
após reiniciar.
]

#horizontalrule

== Contactos e Suporte
<contactos-e-suporte>
#strong[Projeto:] LI4 --- Grupo 9 \
#strong[Instituição:] Universidade do Minho \
#strong[Ano letivo:] 2025/2026

#horizontalrule

#emph[Documento gerado em 2026-05-27]
