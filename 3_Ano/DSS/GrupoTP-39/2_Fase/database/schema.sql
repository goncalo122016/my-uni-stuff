-- =====================================================
-- UTILIZADORES (para autenticação)
-- =====================================================
-- CREATE TABLE IF NOT EXISTS utilizador (
--     id TEXT PRIMARY KEY,
--     username TEXT NOT NULL UNIQUE,
--     password TEXT NOT NULL,
--     cargo TEXT NOT NULL,
--     criacao DATETIME DEFAULT CURRENT_TIMESTAMP
-- );

-- =====================================================
-- MENUS
-- =====================================================
CREATE TABLE IF NOT EXISTS menu (
    id TEXT PRIMARY KEY,
    designacao TEXT NOT NULL,
    prato_id TEXT NOT NULL,
    bebida_id TEXT NOT NULL,
    preco REAL NOT NULL,
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (prato_id) REFERENCES produto(id),
    FOREIGN KEY (bebida_id) REFERENCES produto(id)
);
CREATE TABLE IF NOT EXISTS restaurante (
    id TEXT PRIMARY KEY,
    nome TEXT NOT NULL,
    localizacao TEXT,
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- =====================================================
-- Restaurante Management System - SQLite Schema
-- =====================================================

-- =====================================================
-- PRODUTOS
-- =====================================================

CREATE TABLE IF NOT EXISTS produto (
    id TEXT PRIMARY KEY,
    designacao TEXT NOT NULL,
    preco REAL NOT NULL,
    tipo TEXT NOT NULL CHECK(tipo IN ('PRATO', 'BEBIDA', 'MENU')),
    -- Novos atributos
    disponivel INTEGER NOT NULL DEFAULT 1, -- 1=true, 0=false
    volume_l REAL, -- para bebidas
    stock INTEGER, -- para bebidas
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS ingrediente (
    id TEXT PRIMARY KEY,
    nome TEXT NOT NULL,
    quantidade REAL NOT NULL,
    unidade TEXT NOT NULL,
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS prato_ingrediente (
    prato_id TEXT NOT NULL,
    ingrediente_id TEXT NOT NULL,
    quantidade_usada REAL NOT NULL,
    PRIMARY KEY (prato_id, ingrediente_id),
    FOREIGN KEY (prato_id) REFERENCES produto(id) ON DELETE CASCADE,
    FOREIGN KEY (ingrediente_id) REFERENCES ingrediente(id) ON DELETE CASCADE
);

-- Relação PRATO-OPCAO (ingredientes opcionais)
CREATE TABLE IF NOT EXISTS prato_opcao (
    prato_id TEXT NOT NULL,
    ingrediente_id TEXT NOT NULL,
    -- quantidade opcional aplicável quando a opção é escolhida
    quantidade_opcional REAL,
    PRIMARY KEY (prato_id, ingrediente_id),
    FOREIGN KEY (prato_id) REFERENCES produto(id) ON DELETE CASCADE,
    FOREIGN KEY (ingrediente_id) REFERENCES ingrediente(id) ON DELETE CASCADE
);

-- =====================================================
-- PEDIDOS
-- =====================================================

CREATE TABLE IF NOT EXISTS pedido (
    id TEXT PRIMARY KEY,
    data_hora DATETIME NOT NULL,
    pronto INTEGER NOT NULL DEFAULT 0,
    pago INTEGER NOT NULL DEFAULT 0,
    metodo_pagamento TEXT,
    total REAL NOT NULL DEFAULT 0.0,
    restaurante_id TEXT REFERENCES restaurante(id),
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS item_pedido (
    id TEXT PRIMARY KEY,
    pedido_id TEXT NOT NULL,
    produto_id TEXT NOT NULL,
    quantidade INTEGER NOT NULL,
    preco_unitario REAL NOT NULL,
    FOREIGN KEY (pedido_id) REFERENCES pedido(id) ON DELETE CASCADE,
    FOREIGN KEY (produto_id) REFERENCES produto(id) ON DELETE CASCADE
);

-- =====================================================
-- ENTREGAS
-- =====================================================

CREATE TABLE IF NOT EXISTS entrega (
    id TEXT PRIMARY KEY,
    pedido_id TEXT NOT NULL,
    tipo_entrega TEXT NOT NULL CHECK(tipo_entrega IN ('DELIVERY', 'BALCAO')),
    estado TEXT NOT NULL DEFAULT 'PENDENTE' CHECK(estado IN ('PENDENTE', 'EM_PREPARACAO', 'PRONTO', 'ENTREGUE')),
    endereco TEXT,
    data_criacao DATETIME DEFAULT CURRENT_TIMESTAMP,
    data_entrega DATETIME,
    FOREIGN KEY (pedido_id) REFERENCES pedido(id) ON DELETE CASCADE
);

-- =====================================================
-- ESTATÍSTICAS
-- =====================================================


CREATE TABLE IF NOT EXISTS perfil (
    id TEXT PRIMARY KEY,
    nome TEXT NOT NULL,
    username TEXT NOT NULL UNIQUE,
    password TEXT NOT NULL,
    cargo TEXT NOT NULL,
    restaurante_id TEXT,
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (restaurante_id) REFERENCES restaurante(id) ON DELETE SET NULL
);

CREATE TABLE IF NOT EXISTS estatisticas (
    id TEXT PRIMARY KEY,
    tipo TEXT NOT NULL CHECK(tipo IN ('RESTAURANTE', 'CADEIA')),
    total_pedidos INTEGER DEFAULT 0,
    total_receita REAL DEFAULT 0.0,
    pedidos_entregues INTEGER DEFAULT 0,
    rating_medio REAL DEFAULT 0.0,
    criacao DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- =====================================================
-- Índices para melhor performance
-- =====================================================

CREATE INDEX idx_pedido_data ON pedido(data_hora);
CREATE INDEX idx_entrega_pedido ON entrega(pedido_id);
CREATE INDEX idx_item_pedido_pedido ON item_pedido(pedido_id);
CREATE INDEX idx_produto_tipo ON produto(tipo);

-- =============================================
-- ALTER TABLE para bases já existentes
-- (Ignorar erros de coluna duplicada)
-- =============================================
ALTER TABLE produto ADD COLUMN disponivel INTEGER NOT NULL DEFAULT 1;
ALTER TABLE produto ADD COLUMN volume_l REAL;
ALTER TABLE produto ADD COLUMN stock INTEGER;
