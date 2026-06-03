	-- ========================================================================
	-- Base de Dados: BeloCars
	-- Descrição: Criação de um plano de Indexação para otimização de queries
	-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
	-- Data: Junho de 2025
	-- ========================================================================

	USE BeloCars;

-- =========================================
-- Índices essenciais para a Tabela: Aluguer
-- =========================================

-- Acesso frequente por cliente
CREATE INDEX idx_Aluguer_ID_Cliente ON Aluguer(ID_Cliente);

-- Acesso por intervalo de datas (ex: históricos)
CREATE INDEX idx_Aluguer_DataInicio ON Aluguer(DataInicio);

-- =========================================
-- Índices essenciais para a Tabela: Veiculo
-- =========================================

-- Consulta por stand (veículos de um stand)
CREATE INDEX idx_Veiculo_ID_Stand ON Veiculo(ID_Stand);

-- Pesquisa por disponibilidade
CREATE INDEX idx_Veiculo_Disponivel ON Veiculo(Disponivel);

-- Consulta rápida por marca e modelo
CREATE INDEX idx_Veiculo_Marca_Modelo ON Veiculo(Marca, Modelo);

-- =========================================
-- Índices essenciais para a Tabela: Funcionario
-- =========================================

-- Acesso por stand (funcionários de um stand)
CREATE INDEX idx_Funcionario_ID_Stand ON Funcionario(ID_Stand);