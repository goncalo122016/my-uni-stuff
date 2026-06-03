-- ========================================================================
-- Base de Dados: BeloCars
-- Descrição: Script para apagar dados da base de dados BeloCars respeitando as dependências
-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
-- Data: Junho de 2025
-- ========================================================================

USE BeloCars;

-- SET SQL_SAFE_UPDATES = 0;

-- ========================================================
-- Ordem correta para a Remoção respeitando os dependências
-- ========================================================

DELETE FROM Aluguer;
DELETE FROM Contactos_Funcionario;
DELETE FROM Funcionario;
DELETE FROM Contactos_Cliente;
DELETE FROM Cliente;
DELETE FROM Contactos_Stand;
DELETE FROM Veiculo;
DELETE FROM Stand;
DELETE FROM Localizacao;
