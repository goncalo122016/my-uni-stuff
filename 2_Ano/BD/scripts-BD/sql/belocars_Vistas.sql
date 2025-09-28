-- ========================================================================
-- Base de Dados: BeloCars
-- Descrição: Criação de Views para facilitar consultas frequentes
-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
-- Data: Junho de 2025
-- ========================================================================

USE BeloCars;

-- =========================================
-- View: vwVeiculosGeral
-- Descrição: Retorna todos os campos da tabela Veiculo
-- Uso: Consulta geral sobre veículos
-- =========================================
CREATE VIEW vwVeiculosGeral AS
SELECT *
FROM Veiculo;


-- =========================================
-- View: vwAlugueresGeral
-- Descrição: Informações gerais dos alugueres
-- Campos principais: Carro (Matricula), Cliente, Funcionario, DataInicio, DataFim, Valor, MetodoPagamento
-- Uso: Análise e relatório de alugueres
-- =========================================
CREATE VIEW vwAlugueresGeral AS
SELECT 
    Matricula AS Carro, 
    ID_Cliente AS Cliente,
    ID_Funcionario AS Funcionario, 
    DataInicio,
    DataFim, 
    Valor, 
    MetodoPagamento 
FROM Aluguer;


-- =========================================
-- View: vwFuncionariosDesempenho
-- Descrição: Lista funcionários com o número de alugueres que realizaram
-- Uso: Avaliar desempenho por quantidade de alugueres efetuados
-- Ordenação: Decrescente pelo número de alugueres
-- =========================================
CREATE VIEW vwFuncionariosDesempenho AS
SELECT 
    F.Id AS Funcionario, 
    F.Nome, 
    COUNT(AL.Id) AS Alugueres 
FROM Funcionario AS F
LEFT JOIN Aluguer AS AL ON F.Id = AL.ID_Funcionario
GROUP BY F.Id, F.Nome
ORDER BY Alugueres DESC;


-- =========================================
-- View: vwAlugueresCliente
-- Descrição: Lista alugueres com informações dos veículos para cada cliente
-- Campos principais: ID_Cliente, Matricula, DataInicio, DataFim, Marca, Modelo
-- Uso: Consultas detalhadas de alugueres por cliente
-- =========================================
CREATE VIEW vwAlugueresCliente AS
SELECT 
    AL.ID_Cliente, 
    AL.Matricula,
    AL.DataInicio, 
    AL.DataFim,
    V.Marca, 
    V.Modelo 
FROM Aluguer AS AL
JOIN Veiculo AS V ON AL.Matricula = V.Matricula;


-- =========================================
-- View: vwAlugueresFuncionario
-- Descrição: Lista alugueres com dados de veículos e valores para cada funcionário
-- Campos principais: ID_Funcionario, Matricula, DataInicio, DataFim, Valor
-- Uso: Consultas de alugueres por funcionário
-- =========================================
CREATE VIEW vwAlugueresFuncionario AS  
SELECT 
    ID_Funcionario, 
    Matricula,
    DataInicio, 
    DataFim, 
    Valor
FROM Aluguer;


-- =========================================
-- View: vwFuncionariosGeral
-- Descrição: Retorna todos os campos da tabela Funcionario
-- Uso: Consulta geral sobre funcionários
-- =========================================
CREATE VIEW vwFuncionariosGeral AS
SELECT *
FROM Funcionario;
