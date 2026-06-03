-- =====================================================
-- Restaurantes
-- =====================================================
INSERT INTO restaurante (id, nome, localizacao) VALUES
('REST001', 'Restaurante Central', 'Centro'),
('REST002', 'Restaurante Norte', 'Zona Norte'),
('REST003', 'Restaurante Sul', 'Zona Sul');

-- =====================================================
-- Restaurante Management System - Dados de Teste
-- =====================================================

-- =====================================================
-- INGREDIENTES
-- =====================================================

INSERT INTO ingrediente (id, nome, quantidade, unidade) VALUES
('ING001', 'Frango', 50, 'kg'),
('ING002', 'Arroz', 30, 'kg'),
('ING003', 'Feijão', 20, 'kg'),
('ING004', 'Alface', 15, 'kg'),
('ING005', 'Tomate', 25, 'kg'),
('ING006', 'Cebola', 18, 'kg'),
('ING007', 'Alho', 5, 'kg'),
('ING008', 'Azeite', 10, 'L'),
('ING009', 'Sal', 2, 'kg'),
('ING010', 'Pimenta', 1, 'kg');


-- =====================================================
-- MENUS
-- =====================================================
INSERT INTO menu (id, designacao, prato_id, bebida_id, preco) VALUES
('MENU001', 'Arroz com Frango + Água Natural', 'PROD001', 'PROD004', 14.50),
('MENU002', 'Feijão com Arroz + Suco de Laranja', 'PROD003', 'PROD006', 14.49),
('MENU003', 'Salada Mista + Refrigerante', 'PROD002', 'PROD005', 12.49),
('MENU004', 'Prato Combinado + Água Natural', 'PROD007', 'PROD004', 20.99),
('MENU005', 'Frango Grelhado + Suco de Laranja', 'PROD008', 'PROD006', 20.98);

INSERT INTO produto (id, designacao, preco, tipo, disponivel, volume_l, stock) VALUES
('PROD001', 'Arroz com Frango', 12.50, 'PRATO', 1, NULL, NULL),
('PROD002', 'Salada Mista', 8.99, 'PRATO', 1, NULL, NULL),
('PROD003', 'Feijão com Arroz', 9.50, 'PRATO', 1, NULL, NULL),
('PROD004', 'Água Natural 500ml', 2.00, 'BEBIDA', 1, 0.5, 100),
('PROD005', 'Refrigerante 500ml', 3.50, 'BEBIDA', 1, 0.5, 100),
('PROD006', 'Suco de Laranja', 4.99, 'BEBIDA', 1, 0.33, 50),
('PROD007', 'Prato Combinado', 18.99, 'PRATO', 1, NULL, NULL),
('PROD008', 'Frango Grelhado', 15.99, 'PRATO', 1, NULL, NULL);

-- =====================================================
-- RELAÇÕES PRATO-INGREDIENTE
-- =====================================================

INSERT INTO prato_ingrediente (prato_id, ingrediente_id, quantidade_usada) VALUES
('PROD001', 'ING001', 0.3),
('PROD001', 'ING002', 0.2),
('PROD001', 'ING009', 0.01),
('PROD002', 'ING004', 0.1),
('PROD002', 'ING005', 0.1),
('PROD002', 'ING006', 0.05),
('PROD002', 'ING008', 0.05),
('PROD003', 'ING003', 0.2),
('PROD003', 'ING002', 0.2),
('PROD003', 'ING009', 0.01),
('PROD007', 'ING001', 0.4),
('PROD007', 'ING002', 0.25),
('PROD007', 'ING003', 0.15),
('PROD008', 'ING001', 0.5);

-- =====================================================
-- OPÇÕES POR PRATO
-- =====================================================

INSERT INTO prato_opcao (prato_id, ingrediente_id, quantidade_opcional) VALUES
('PROD001', 'ING004', 0.1),
('PROD001', 'ING001', 0.2),
('PROD001', 'ING002', 0.2), 
('PROD002', 'ING006', 0.05),
('PROD002', 'ING008', 0.05),
('PROD003', 'ING010', 0.01),
('PROD007', 'ING001', 0.2),
('PROD008', 'ING002', 0.2);

-- =====================================================
-- CARGOS E PERFIS
-- =====================================================

INSERT INTO perfil (id, nome, username, password, cargo , restaurante_id) VALUES
('PERF001', 'Admin Sistema', 'admin', 'admin', 'COO', NULL),
('PERF002', 'Maria Santos - Chef', 'chefe', 'chefe123', 'CHEFE', 'REST001'),
('PERF003', 'João Silva - Chef', 'chef2', 'chefe789', 'CHEFE', 'REST002'),
('PERF004', 'Pedro Costa', 'pedro.atendente', 'senha789', 'FUNCIONARIO', 'REST001'),
('PERF005', 'Lucas Ferreira', 'lucas.entregador', 'senha321', 'FUNCIONARIO', 'REST002');

-- =====================================================
-- PEDIDOS DE EXEMPLO
-- =====================================================

-- Pedidos de Exemplo (com restaurante_id)
INSERT INTO pedido (id, data_hora, pronto, pago, metodo_pagamento, total, restaurante_id) VALUES
('PED001', datetime('2025-11-17 12:30:00'), 1, 1, 'CARTAO', 25.50, 'REST001'),
('PED002', datetime('2025-11-17 13:15:00'), 0, 0, 'DINHEIRO', 33.48, 'REST002'),
('PED003', datetime('2025-11-17 14:00:00'), 1, 1, 'CARTAO', 18.98, 'REST003'),
('PED004', datetime('2025-11-18 11:00:00'), 1, 1, 'CARTAO', 22.00, 'REST001'),
('PED005', datetime('2025-11-18 12:00:00'), 0, 0, 'DINHEIRO', 15.00, 'REST002'),
('PED006', datetime('2025-11-18 13:00:00'), 1, 1, 'CARTAO', 30.00, 'REST003');

-- =====================================================
-- ITENS DOS PEDIDOS
-- Itens dos Pedidos (novos exemplos)
INSERT INTO item_pedido (id, pedido_id, produto_id, quantidade, preco_unitario) VALUES
('ITEM008', 'PED004', 'PROD001', 1, 12.50),
('ITEM009', 'PED004', 'PROD004', 2, 2.00),
('ITEM010', 'PED005', 'PROD002', 1, 8.99),
('ITEM011', 'PED005', 'PROD005', 1, 3.50),
('ITEM012', 'PED006', 'PROD003', 2, 9.50),
('ITEM013', 'PED006', 'PROD006', 1, 4.99);
-- =====================================================

INSERT INTO item_pedido (id, pedido_id, produto_id, quantidade, preco_unitario) VALUES
('ITEM001', 'PED001', 'PROD001', 2, 12.50),
('ITEM002', 'PED001', 'PROD004', 1, 2.00),
('ITEM003', 'PED002', 'PROD007', 1, 18.99),
('ITEM004', 'PED002', 'PROD005', 2, 3.50),
('ITEM005', 'PED002', 'PROD006', 1, 4.99),
('ITEM006', 'PED003', 'PROD002', 2, 8.99),
('ITEM007', 'PED003', 'PROD004', 1, 2.00);

-- =====================================================
-- ENTREGAS
-- =====================================================

INSERT INTO entrega (id, pedido_id, tipo_entrega, estado, endereco, data_criacao, data_entrega) VALUES
('ENTREGA001', 'PED001', 'BALCAO', 'ENTREGUE', NULL, datetime('2025-11-17 12:30:00'), datetime('2025-11-17 12:45:00')),
('ENTREGA002', 'PED002', 'DELIVERY', 'EM_PREPARACAO', 'Rua Principal, 123', datetime('2025-11-17 13:15:00'), NULL),
('ENTREGA003', 'PED003', 'BALCAO', 'PRONTO', NULL, datetime('2025-11-17 14:00:00'), NULL);

-- =====================================================
-- ESTATÍSTICAS
-- =====================================================


