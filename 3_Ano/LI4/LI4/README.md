# BELAVISTA — Sistema de Gestão de Cadeia de Lojas

> LI4 · Grupo 9 · Universidade do Minho · 2025/2026

Sistema distribuído de gestão para uma cadeia de lojas de conveniência, composto por dois backends Spring Boot e um frontend Next.js.

## Arquitetura

| Componente | Tecnologia | Porta |
|---|---|---|
| `backend-cadeia` | Spring Boot 3.4 + Kotlin | **8080** |
| `backend-loja` | Spring Boot 3.4 + Kotlin | **8081** |
| `frontend` | Next.js 16 + TypeScript | **3000** |

## Iniciar rapidamente

**Pré-requisitos:** Java 21, Node.js 18+

```bash
# 1. Backend Central (terminal 1)
java -Xmx256m -jar backend-cadeia/build/libs/backend-cadeia-0.0.1-SNAPSHOT.jar

# 2. Backend Loja (terminal 2)
java -Xmx256m -jar backend-loja/build/libs/backend-loja-0.0.1-SNAPSHOT.jar

# 3. Frontend (terminal 3)
cd frontend && npm install && npm run dev
```

Aceder em: **http://localhost:3000**

## Credenciais por defeito

| Backend | Número | Senha | Perfil |
|---|---|---|---|
| cadeia | `ADM001` | `admin123` | Administrador |
| cadeia | `GER001` | `gerente123` | Gestor |
| loja | `ADM001` | `admin123` | Administrador |
| loja | `FUN001` | `func123` | Funcionário |

## Documentação completa

📖 **[GUIA_INSTALACAO_OPERACAO.md](./GUIA_INSTALACAO_OPERACAO.md)** — Guia completo de instalação, configuração, operação e manutenção.

API Swagger disponível em:
- http://localhost:8080/swagger-ui.html (cadeia)
- http://localhost:8081/swagger-ui.html (loja)
