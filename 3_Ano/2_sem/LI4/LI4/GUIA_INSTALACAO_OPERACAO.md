# BELAVISTA — Guia de Instalação, Operação e Manutenção

> Sistema de Gestão de Cadeia de Lojas de Conveniência  
> LI4 · Grupo 9 · Universidade do Minho · 2025/2026

---

## Índice

1. [Visão Geral do Sistema](#1-visão-geral-do-sistema)
2. [Requisitos de Sistema](#2-requisitos-de-sistema)
3. [Instalação](#3-instalação)
   - 3.1 [Clonar o Repositório](#31-clonar-o-repositório)
   - 3.2 [Compilar os Backends](#32-compilar-os-backends)
   - 3.3 [Instalar Dependências do Frontend](#33-instalar-dependências-do-frontend)
4. [Configuração](#4-configuração)
   - 4.1 [backend-cadeia](#41-backend-cadeia)
   - 4.2 [backend-loja](#42-backend-loja)
   - 4.3 [Frontend](#43-frontend)
5. [Iniciar o Sistema](#5-iniciar-o-sistema)
6. [Credenciais de Acesso](#6-credenciais-de-acesso)
7. [Guia de Operação](#7-guia-de-operação)
   - 7.1 [Interface Web (Frontend)](#71-interface-web-frontend)
   - 7.2 [Perfis de Utilizador](#72-perfis-de-utilizador)
   - 7.3 [Fluxos Principais](#73-fluxos-principais)
8. [API REST — Referência Rápida](#8-api-rest--referência-rápida)
   - 8.1 [backend-cadeia (porta 8080)](#81-backend-cadeia-porta-8080)
   - 8.2 [backend-loja (porta 8081)](#82-backend-loja-porta-8081)
9. [Sincronização Cadeia ↔ Loja](#9-sincronização-cadeia--loja)
10. [Base de Dados](#10-base-de-dados)
11. [Manutenção](#11-manutenção)
    - 11.1 [Logs](#111-logs)
    - 11.2 [Backup](#112-backup)
    - 11.3 [Reinício dos Serviços](#113-reinício-dos-serviços)
    - 11.4 [Atualização do Sistema](#114-atualização-do-sistema)
12. [Resolução de Problemas (Troubleshooting)](#12-resolução-de-problemas-troubleshooting)
13. [Paragem do Sistema](#13-paragem-do-sistema)

---

## 1. Visão Geral do Sistema

O **BELAVISTA** é um sistema distribuído de gestão para uma cadeia de lojas de conveniência, composto por três componentes independentes que comunicam entre si:

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

| Componente | Tecnologia | Porta | Responsabilidade |
|---|---|---|---|
| **backend-cadeia** | Spring Boot 3.4 + Kotlin | 8080 | Gestão central: lojas, fornecedores, produtos do catálogo, funcionários, relatórios consolidados |
| **backend-loja** | Spring Boot 3.4 + Kotlin | 8081 | Gestão local: vendas, stock, clientes, devoluções, faturas |
| **frontend** | Next.js 16 + TypeScript | 3000 | Interface web para todos os utilizadores |

**Base de dados:** SQLite (ficheiros `cadeia.db` e `loja.db` na raiz de cada backend).

---

## 2. Requisitos de Sistema

### Software obrigatório

| Software | Versão mínima | Verificação |
|---|---|---|
| **Java (JDK)** | 21 | `java -version` |
| **Node.js** | 18 LTS | `node -v` |
| **npm** | 9 | `npm -v` |

### Software opcional (para desenvolvimento/rebuild)

| Software | Uso |
|---|---|
| **Gradle** | Recompilar os backends (o projeto inclui `gradlew`) |
| **Git** | Clonar e atualizar o repositório |

### Recursos de hardware (mínimos)

| Recurso | Mínimo recomendado |
|---|---|
| RAM | 1 GB disponível |
| Disco | 500 MB |
| CPU | Dual-core |
| Rede | Comunicação local entre as três portas |

### Sistema operativo

Compatível com **Linux**, **macOS** e **Windows** (com WSL ou PowerShell).

---

## 3. Instalação

### 3.1 Clonar o Repositório

```bash
git clone <url-do-repositorio>
cd LI4
```

### 3.2 Compilar os Backends

> **Nota:** Os ficheiros JAR já compilados encontram-se em  
> `backend-cadeia/build/libs/` e `backend-loja/build/libs/`.  
> Se não for necessário recompilar, avance para a secção 5.

Para recompilar (requer Java 21):

```bash
# Na raiz do projeto
./gradlew :backend-cadeia:bootJar :backend-loja:bootJar
```

No Windows (sem WSL):
```cmd
gradlew.bat :backend-cadeia:bootJar :backend-loja:bootJar
```

Os JARs são gerados em:
- `backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar`
- `backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar`

### 3.3 Instalar Dependências do Frontend

```bash
cd frontend
npm install
```

---

## 4. Configuração

### 4.1 backend-cadeia

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

> **Importante:** `ddl-auto: create` apaga e recria as tabelas a cada arranque.  
> Em produção ou para preservar dados, alterar para `ddl-auto: update`.

### 4.2 backend-loja

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

> Para instalar o sistema em **múltiplas lojas físicas**, cada instância do `backend-loja`
> deve ter um `loja.id` diferente e apontar `cadeia.url` para o servidor central correto.

### 4.3 Frontend

Ficheiro: `frontend/next.config.ts`

```typescript
// As chamadas ao frontend são reencaminhadas para os backends:
"/loja-api/*"    →  http://localhost:8081/api/*   (backend-loja)
"/cadeia-api/*"  →  http://localhost:8080/api/*   (backend-cadeia)
```

Se os backends estiverem em servidores diferentes, alterar os valores de `destination` neste ficheiro.

---

## 5. Iniciar o Sistema

Os três componentes devem ser iniciados **pela seguinte ordem**:

### Passo 1 — Iniciar o backend-cadeia

```bash
java -Xmx256m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar
```

Aguardar até aparecer no terminal:
```
Started CadeiaApplication in X.XXX seconds
```

### Passo 2 — Iniciar o backend-loja

Abrir um **novo terminal** e executar:

```bash
java -Xmx256m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar
```

Aguardar até aparecer:
```
Started LojaApplication in X.XXX seconds
```

### Passo 3 — Iniciar o Frontend

Abrir um **terceiro terminal**:

```bash
cd frontend

# Modo produção (recomendado):
npm run build
npm run start

# Modo desenvolvimento:
npm run dev
```

### Verificação

Após iniciar todos os componentes, aceder a:

| URL | Componente | Esperado |
|---|---|---|
| http://localhost:3000 | Frontend | Página de login BELAVISTA |
| http://localhost:8080/swagger-ui.html | API Cadeia | Documentação Swagger |
| http://localhost:8081/swagger-ui.html | API Loja | Documentação Swagger |

---

## 6. Credenciais de Acesso

### backend-cadeia (Sede da Cadeia)

| Número | Senha | Perfil |
|---|---|---|
| `ADM001` | `admin123` | Administrador |
| `GER001` | `gerente123` | Gestor |

### backend-loja (Loja Local)

| Número | Senha | Perfil |
|---|---|---|
| `ADM001` | `admin123` | Administrador |
| `GER001` | `gerente123` | Gestor |
| `FUN001` | `func123` | Funcionário |
| `FUN002` | `func123` | Funcionário |

> **Segurança:** As senhas estão armazenadas em texto simples na base de dados.  
> Em ambiente de produção real, devem ser substituídas por hashes (ex.: bcrypt).

---

## 7. Guia de Operação

### 7.1 Interface Web (Frontend)

Aceder a **http://localhost:3000** num browser moderno (Chrome, Firefox, Edge).

O sistema deteta automaticamente se o utilizador pertence à **cadeia** ou à **loja** e apresenta o dashboard correspondente.

### 7.2 Perfis de Utilizador

#### Administrador da Cadeia (`ADM`)
- Gerir lojas da cadeia (criar, consultar)
- Gerir funcionários
- Gerir fornecedores
- Gerir catálogo de produtos central
- Consultar estatísticas consolidadas
- Gerar relatórios da cadeia
- Consultar sincronizações

#### Gestor da Cadeia (`GER`)
- Consultar estatísticas e relatórios
- Consultar lojas e fornecedores

#### Administrador da Loja (`ADM`)
- Gerir stock local (entradas, ajustes)
- Gerir funcionários da loja
- Gerir clientes
- Consultar vendas e histórico
- Processar devoluções
- Forçar sincronização com a cadeia

#### Gestor da Loja (`GER`)
- Consultar stock e produtos
- Consultar vendas e histórico

#### Funcionário da Loja (`FUN`)
- Registar vendas
- Emitir faturas
- Registar devoluções
- Consultar produtos e stock

### 7.3 Fluxos Principais

#### Registar uma Venda
1. Autenticar como Funcionário ou Gestor da Loja
2. Navegar para **Vendas → Nova Venda**
3. Selecionar cliente (ou venda anónima)
4. Adicionar linhas de venda (produto + quantidade)
5. Confirmar a venda
6. Emitir fatura se necessário

#### Gerir Stock
1. Autenticar como Administrador da Loja
2. Navegar para **Stock**
3. Para entrada de mercadoria: **Entrada de Stock** → produto + quantidade
4. Para ajuste de inventário: **Ajuste de Stock** → produto + nova quantidade

#### Registar uma Devolução
1. Autenticar como Funcionário ou superior
2. Navegar para **Vendas** → localizar a venda
3. Selecionar **Registar Devolução**
4. Indicar os itens a devolver

#### Consultar Relatórios Consolidados (Cadeia)
1. Autenticar na cadeia como ADM ou GER
2. Navegar para **Central → Relatórios**
3. Criar novo relatório ou consultar os existentes
4. Consultar estatísticas por loja

#### Adicionar uma Nova Loja à Cadeia
1. Autenticar na cadeia como ADM
2. Navegar para **Cadeia → Lojas → Nova Loja**
3. Preencher nome e localização
4. Configurar uma nova instância do `backend-loja` com o `loja.id` correspondente

---

## 8. API REST — Referência Rápida

A documentação interativa completa está disponível via Swagger:
- Cadeia: http://localhost:8080/swagger-ui.html
- Loja: http://localhost:8081/swagger-ui.html

### 8.1 backend-cadeia (porta 8080)

| Método | Endpoint | Descrição |
|---|---|---|
| `POST` | `/api/auth/login` | Autenticação |
| `GET` | `/api/cadeia` | Obter informação da cadeia |
| `POST` | `/api/cadeia` | Criar cadeia |
| `POST` | `/api/cadeia/{id}/lojas` | Associar loja à cadeia |
| `GET` | `/api/lojas` | Listar todas as lojas |
| `GET` | `/api/lojas/{id}` | Obter loja por ID |
| `GET` | `/api/lojas/{id}/estatisticas` | Estatísticas de uma loja |
| `POST` | `/api/lojas/{id}/fornecedores` | Associar fornecedor a loja |
| `POST` | `/api/lojas/{id}/sync` | Forçar sync de uma loja |
| `GET` | `/api/fornecedores` | Listar fornecedores |
| `GET` | `/api/fornecedores/{id}` | Obter fornecedor |
| `POST` | `/api/fornecedores` | Criar fornecedor |
| `GET` | `/api/produtos` | Listar produtos do catálogo |
| `GET` | `/api/produtos/{id}` | Obter produto |
| `POST` | `/api/produtos` | Criar produto |
| `PUT` | `/api/produtos/{id}` | Atualizar produto |
| `GET` | `/api/utilizadores` | Listar funcionários |
| `GET` | `/api/utilizadores/{id}` | Obter funcionário |
| `POST` | `/api/utilizadores` | Criar funcionário |
| `PUT` | `/api/utilizadores/{id}` | Atualizar funcionário |
| `PATCH` | `/api/utilizadores/{id}/loja` | Atribuir loja ao funcionário |
| `GET` | `/api/central/estatisticas` | Estatísticas consolidadas |
| `POST` | `/api/central/relatorios` | Gerar relatório |
| `GET` | `/api/central/relatorios` | Listar relatórios |
| `GET` | `/api/central/sincronizacoes` | Listar sincronizações |
| `POST` | `/api/sync/importar` | Receber dados de sincronização *(chamado pela loja)* |

### 8.2 backend-loja (porta 8081)

| Método | Endpoint | Descrição |
|---|---|---|
| `POST` | `/api/auth/login` | Autenticação |
| `GET` | `/api/produtos` | Listar produtos locais |
| `GET` | `/api/produtos/{id}` | Obter produto |
| `GET` | `/api/stock` | Listar stock |
| `GET` | `/api/stock/produto/{produtoId}` | Stock de um produto |
| `POST` | `/api/stock/entrada` | Registar entrada de stock |
| `POST` | `/api/stock/ajuste` | Ajustar stock |
| `GET` | `/api/clientes` | Listar clientes |
| `GET` | `/api/clientes/{id}` | Obter cliente |
| `POST` | `/api/clientes` | Criar cliente |
| `PUT` | `/api/clientes/{id}` | Atualizar cliente |
| `GET` | `/api/utilizadores` | Listar funcionários |
| `GET` | `/api/utilizadores/{id}` | Obter funcionário |
| `POST` | `/api/utilizadores` | Criar funcionário |
| `PUT` | `/api/utilizadores/{id}` | Atualizar funcionário |
| `POST` | `/api/vendas` | Registar venda |
| `POST` | `/api/vendas/{id}/fatura` | Emitir fatura |
| `POST` | `/api/vendas/{id}/devolucao` | Registar devolução |
| `GET` | `/api/vendas/historico` | Histórico de vendas |
| `POST` | `/api/vendas/sync` | Forçar sincronização com cadeia |

---

## 9. Sincronização Cadeia ↔ Loja

O `backend-loja` envia automaticamente um resumo das vendas do dia para o `backend-cadeia`.

### Sincronização Automática
- **Horário:** Todos os dias às **23:55**
- **Dados enviados:** total de vendas, número de transações, produtos vendidos

### Sincronização Manual
Via API:
```bash
POST http://localhost:8081/api/vendas/sync
```

Via interface web:
- Autenticar como ADM na loja → **Sync com Cadeia**

### O que é sincronizado
- Totais financeiros das vendas do dia
- Número de transações
- Os dados detalhados (linhas de venda, clientes) **não** são enviados — ficam apenas na loja

---

## 10. Base de Dados

### Localização dos ficheiros

| Ficheiro | Localização | Conteúdo |
|---|---|---|
| `cadeia.db` | `backend-cadeia/cadeia.db` | Dados centrais da cadeia |
| `loja.db` | `backend-loja/loja.db` | Dados locais da loja |

### Tabelas principais — cadeia.db

| Tabela | Descrição |
|---|---|
| `cadeia` | Informação da cadeia |
| `loja` | Lojas registadas |
| `produto` | Catálogo de produtos central |
| `categoria` | Categorias de produtos |
| `fornecedor` | Fornecedores |
| `funcionario` | Funcionários da cadeia |
| `relatorio` | Relatórios gerados |
| `sincronizacao_loja` | Registos de sincronização recebidos |

### Tabelas principais — loja.db

| Tabela | Descrição |
|---|---|
| `produto` | Produtos disponíveis na loja |
| `stock` | Níveis de stock por produto |
| `cliente` | Clientes registados |
| `funcionario` | Funcionários da loja |
| `venda` | Vendas realizadas |
| `linha_de_venda` | Linhas de cada venda |
| `fatura` | Faturas emitidas |
| `devolucao` | Devoluções registadas |
| `historico_de_vendas` | Histórico consolidado |

### Acesso direto à base de dados (SQLite)

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

---

## 11. Manutenção

### 11.1 Logs

Os logs são apresentados no terminal onde cada processo foi iniciado.

**Nível de log por defeito:** `DEBUG` para `com.li4.*`

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

### 11.2 Backup

**Backup manual das bases de dados:**

```bash
# Criar diretório de backups
mkdir -p backups

# Backup da cadeia
cp backend-cadeia/cadeia.db backups/cadeia-$(date +%Y%m%d-%H%M%S).db

# Backup da loja
cp backend-loja/loja.db backups/loja-$(date +%Y%m%d-%H%M%S).db
```

> Fazer o backup com os backends **parados** ou usando o comando SQLite `.backup`:
> ```bash
> sqlite3 backend-cadeia/cadeia.db ".backup backups/cadeia-backup.db"
> ```

**Recomendação:** Agendar backups diários (ex.: via `cron` no Linux):
```bash
# Editar crontab: crontab -e
# Backup todos os dias às 02:00
0 2 * * * cp /caminho/para/LI4/backend-cadeia/cadeia.db /backups/cadeia-$(date +\%Y\%m\%d).db
0 2 * * * cp /caminho/para/LI4/backend-loja/loja.db /backups/loja-$(date +\%Y\%m\%d).db
```

### 11.3 Reinício dos Serviços

**Parar os serviços:**
```bash
# Encontrar os PIDs
ps aux | grep "backend-cadeia\|backend-loja"

# Terminar os processos
kill <PID-cadeia>
kill <PID-loja>
```

**Reiniciar:**
```bash
# Reiniciar pela ordem correta: cadeia → loja → frontend
java -Xmx256m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar &
sleep 10
java -Xmx256m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar &
cd frontend && npm run start &
```

### 11.4 Atualização do Sistema

1. **Parar todos os serviços**
2. **Fazer backup das bases de dados** (ver 11.2)
3. **Atualizar o código:**
   ```bash
   git pull origin main
   ```
4. **Recompilar os backends:**
   ```bash
   ./gradlew :backend-cadeia:bootJar :backend-loja:bootJar
   ```
5. **Atualizar dependências do frontend:**
   ```bash
   cd frontend && npm install
   npm run build
   ```
6. **Alterar `ddl-auto` para `update`** nos ficheiros `application.yml` de ambos os backends (para preservar dados existentes)
7. **Reiniciar os serviços** (ver 11.3)

---

## 12. Resolução de Problemas (Troubleshooting)

### O backend não inicia — `Port 8080 already in use`

```bash
# Identificar o processo que usa a porta
lsof -i :8080     # Linux/macOS
netstat -ano | findstr :8080   # Windows

# Terminar o processo
kill -9 <PID>
```

### Erro de ligação entre loja e cadeia

Verificar:
1. O `backend-cadeia` está em execução (http://localhost:8080/swagger-ui.html acessível)
2. `cadeia.url` em `backend-loja/application.yml` aponta para o endereço correto
3. Firewall não bloqueia a porta 8080

### Base de dados apagada a cada reinício

O `ddl-auto: create` **apaga e recria** o schema a cada arranque. Para preservar dados:

```yaml
# Em application.yml (ambos os backends)
jpa:
  hibernate:
    ddl-auto: update   # ← alterar de "create" para "update"
```

### Frontend não consegue comunicar com os backends

Verificar `frontend/next.config.ts`:
```typescript
destination: "http://localhost:8081/api/:path*"  // IP/porta corretos?
destination: "http://localhost:8080/api/:path*"  // IP/porta corretos?
```

Após alterar o ficheiro, recompilar o frontend: `npm run build && npm run start`

### Erro de autenticação — `401 Unauthorized`

- Verificar que o número e senha estão corretos (ver secção 6)
- Confirmar que o backend correto está a ser usado (cadeia vs. loja)
- Os dados são reiniciados se `ddl-auto: create` estiver activo — as credenciais por defeito são restauradas

### Sincronização falha

1. Verificar que ambos os backends estão em execução
2. Verificar os logs do `backend-loja` para mensagens de erro
3. Forçar sync manual: `POST http://localhost:8081/api/vendas/sync`
4. Verificar em `backend-cadeia`: `GET http://localhost:8080/api/central/sincronizacoes`

### Memória insuficiente ao correr ambos os backends

Cada backend é iniciado com `-Xmx256m` (256 MB máximo). Para reduzir o consumo:
```bash
java -Xmx128m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar
java -Xmx128m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar
```

---

## 13. Paragem do Sistema

Para parar o sistema de forma limpa:

```bash
# 1. Parar o frontend (Ctrl+C no terminal, ou:)
kill $(lsof -ti :3000)

# 2. Parar o backend-loja (aguardar que a sync pendente termine)
kill $(lsof -ti :8081)

# 3. Parar o backend-cadeia
kill $(lsof -ti :8080)
```

> A paragem do `backend-loja` antes das 23:55 num dia com vendas **não** perde dados —  
> as vendas ficam guardadas na base de dados local e a sync pode ser forçada manualmente  
> após reiniciar.

---

## Contactos e Suporte

**Projeto:** LI4 — Grupo 9  
**Instituição:** Universidade do Minho  
**Ano letivo:** 2025/2026

---

*Documento gerado em 2026-05-27*
