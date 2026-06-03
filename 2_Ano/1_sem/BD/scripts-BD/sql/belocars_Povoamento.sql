-- ========================================================================
-- Base de Dados: BeloCars
-- Descrição: Inserção, eliminação e visualização de dados no sistema de gestão de veículos
-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
-- Data: Junho de 2025
-- ========================================================================

USE BeloCars;

-- ========================================================
-- Entidade: Localizacao
-- ========================================================
INSERT INTO Localizacao (Id, Endereco, CodigoPostal)
VALUES
(1, 'Rua Central, 123', '1000-001'),
(2, 'Av. das Laranjeiras, 45', '2000-002'),
(3, 'Rua das Oliveiras, 10', '3000-003'),
(4, 'Praça da República, 50', '4000-004'),
(5, 'Estrada Nacional 1, Km 120', '5000-005');

SELECT * FROM Localizacao;

-- ========================================================
-- Entidade: Stand
-- ========================================================
INSERT INTO Stand (Id, Nome, NIF, ID_Localizacao)
VALUES
(1, 'BeloCars Lisboa', 123456789, 1),
(2, 'BeloCars Braga', 987654321, 2),
(3, 'BeloCars Viseu', 456789123, 3);

SELECT * FROM Stand;

-- ========================================================
-- Entidade: Contactos_Stand
-- ========================================================
INSERT INTO Contactos_Stand (ID_Stand, Email, Telefone)
VALUES
(1, 'lisboa@belocars.pt', '912345678'),
(2, 'braga@belocars.pt', '934567890'),
(3, 'viseu@belocars.pt', '938112233');

SELECT * FROM Contactos_Stand;

-- ========================================================
-- Entidade: Cliente
-- ========================================================
INSERT INTO Cliente (ID, Nome, DataNascimento, NIF)
VALUES
(1, 'João Silva', '1985-06-15', 123456780),
(2, 'Maria Oliveira', '1990-10-20', 234567890),
(3, 'Rui Gonçalves', '1975-03-05', 345678901),
(4, 'Inês Martins', '1995-12-30', 456789012),
(5, 'Pedro Ferreira', '1980-09-10', 567890123);

SELECT * FROM Cliente;

-- ========================================================
-- Entidade: Contactos_Cliente
-- ========================================================
INSERT INTO Contactos_Cliente (ID_Cliente, Email, Telefone)
VALUES
(1, 'joao.silva@email.com', '911223344'),
(2, 'maria.oliveira@email.com', '966778899'),
(3, 'rui.goncalves@email.com', '922334455'),
(4, 'ines.martins@email.com', '933445566'),
(5, 'pedro.ferreira@email.com', '944556677');

SELECT * FROM Contactos_Cliente;

-- ========================================================
-- Entidade: Funcionario
-- ========================================================
INSERT INTO Funcionario (Id, Nome, Departamento, Funcao, ID_Stand)
VALUES
(1, 'Carlos Mendes', 'Comercial', 'Vendedor', 1),
(2, 'Ana Costa', 'Administração', 'Gestora', 2),
(3, 'Miguel Rocha', 'Manutenção', 'Mecânico', 3),
(4, 'Sofia Ribeiro', 'Comercial', 'Vendedora', 1),
(5, 'Tiago Lopes', 'Administração', 'Contabilista', 2);

SELECT * FROM Funcionario;

-- ========================================================
-- Entidade: Contactos_Funcionario
-- ========================================================
INSERT INTO Contactos_Funcionario (ID_Funcionario, Email, Telefone)
VALUES
(1, 'carlos.mendes@belocars.pt', '913456789'),
(2, 'ana.costa@belocars.pt', '923456789'),
(3, 'miguel.rocha@belocars.pt', '945667788'),
(4, 'sofia.ribeiro@belocars.pt', '956778899'),
(5, 'tiago.lopes@belocars.pt', '967889900');

SELECT * FROM Contactos_Funcionario;

-- ========================================================
-- Entidade: Veiculo
-- ========================================================
INSERT INTO Veiculo (Matricula, NPassageiros, Tara, CargaMaxima, Transmissao, Combustivel, Consumo, Modelo, Ano, Marca, Disponivel, PrecoDiario, Tipo, ID_Stand)
VALUES
('00-AA-11', 5, NULL, NULL, 'M', 'G', 6.5, 'Civic', 2020, 'Honda', TRUE, 45.00, 'P', 1),
('11-BB-22', NULL, 1500, 2500, 'A', 'D', 7.8, 'Sprinter', 2021, 'Mercedes', TRUE, 60.00, 'M', 2),
('22-CC-33', 5, 1100, 2400, 'M', 'G', 5.8, 'Polo', 2019, 'Volkswagen', TRUE, 40.00, 'PM', 3),
('33-DD-44', 7, NULL, NULL, 'A', 'D', 6.9, 'Sharan', 2022, 'Volkswagen', TRUE, 70.00, 'P', 1),
('44-EE-55', NULL, 1300, 3500, 'M', 'E', 0.0, 'eSprinter', 2023, 'Mercedes', TRUE, 80.00, 'M', 2);

SELECT * FROM Veiculo;

-- ========================================================
-- Entidade: Aluguer
-- ========================================================
INSERT INTO Aluguer (Id, MetodoPagamento, Valor, DataInicio, DataFim, ID_Cliente, Matricula, ID_Funcionario)
VALUES
(1, 'Cartão Crédito', 135.00, '2024-05-01', '2024-05-04', 1, '00-AA-11', 1),
(2, 'MB Way', 180.00, '2024-05-10', '2024-05-13', 2, '11-BB-22', 2),
(3, 'Cartão Débito', 120.00, '2024-06-01', '2024-06-04', 3, '22-CC-33', 3),
(4, 'Paypal', 210.00, '2024-06-10', '2024-06-14', 4, '33-DD-44', 4),
(5, 'Transferência', 190.00, '2024-06-20', '2024-06-23', 5, '44-EE-55', 5);

SELECT * FROM Aluguer;
