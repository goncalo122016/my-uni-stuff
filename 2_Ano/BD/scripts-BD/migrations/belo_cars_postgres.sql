-- ==========================================================================
-- Script: belo_cars_postgres.sql
-- Base de Dados: postgres
-- Tabelas e Dados equivalentes ao sistema BeloCars
-- ==========================================================================

DROP TABLE IF EXISTS Aluguer, Veiculo, Contactos_Funcionario, Funcionario, Contactos_Cliente, Cliente, Contactos_Stand, Stand, Localizacao CASCADE;

-- ========================================================
-- Tabela: Localizacao
-- Descrição: Armazena os endereços físicos associados aos Stands
-- ========================================================
CREATE TABLE IF NOT EXISTS Localizacao (
  Id INT,
  Endereco VARCHAR(50),
  CodigoPostal VARCHAR(10),
  PRIMARY KEY (Id)
);

-- ========================================================
-- Tabela: Stand
-- Descrição: Representa os stands de veículos da empresa
-- Cada stand está associado a uma localização (1:1)
-- ========================================================
CREATE TABLE IF NOT EXISTS Stand (
  Id INT NOT NULL,
  Nome VARCHAR(50) NOT NULL,
  NIF INT NOT NULL UNIQUE,
  ID_Localizacao INT NOT NULL UNIQUE,
  PRIMARY KEY (Id),
  CONSTRAINT fk_Stand_Localizacao
    FOREIGN KEY (ID_Localizacao)
    REFERENCES Localizacao(Id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

-- ========================================================
-- Tabela: Contactos_Stand
-- Descrição: Contactos associados a cada Stand (1:1)
-- Remoção em cascata para consistência de dados
-- ========================================================
CREATE TABLE IF NOT EXISTS Contactos_Stand (
  ID_Stand INT NOT NULL,
  Email VARCHAR(50) NOT NULL UNIQUE,
  Telefone VARCHAR(16) NOT NULL UNIQUE,
  PRIMARY KEY (ID_Stand),
  CONSTRAINT fk_ContactosStand_Stand
    FOREIGN KEY (ID_Stand)
    REFERENCES Stand(Id)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);

-- ========================================================
-- Tabela: Cliente
-- Descrição: Regista os dados dos Clientes que alugam Veículos
-- ========================================================
CREATE TABLE IF NOT EXISTS Cliente (
  ID INT NOT NULL,
  Nome VARCHAR(50) NOT NULL,
  DataNascimento DATE NOT NULL,
  NIF INT UNIQUE,
  PRIMARY KEY (ID)
);

-- ========================================================
-- Tabela: Contactos_Cliente
-- Descrição: Contactos associados a Clientes (1:1)
-- Remoção em cascata garante integridade
-- ========================================================
CREATE TABLE IF NOT EXISTS Contactos_Cliente (
  ID_Cliente INT NOT NULL,
  Email VARCHAR(50) UNIQUE NOT NULL,
  Telefone VARCHAR(16) UNIQUE NOT NULL,
  PRIMARY KEY (ID_Cliente),
  CONSTRAINT fk_ContactosCliente_Cliente
    FOREIGN KEY (ID_Cliente)
    REFERENCES Cliente(ID)
    ON DELETE CASCADE
    ON UPDATE CASCADE
);

-- ========================================================
-- Tabela: Funcionario
-- Descrição: Contém os dados dos Funcionários dos Stands
-- Cada Funcionário está associado a um Stand (N:1)
-- ========================================================
CREATE TABLE IF NOT EXISTS Funcionario (
  Id INT NOT NULL,
  Nome VARCHAR(50) NOT NULL,
  Departamento VARCHAR(50),
  Funcao VARCHAR(50),
  ID_Stand INT NOT NULL,
  PRIMARY KEY (Id),
  CONSTRAINT fk_Funcionario_Stand
    FOREIGN KEY (ID_Stand)
    REFERENCES Stand(Id)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

-- ========================================================
-- Tabela: Contactos_Funcionario
-- Descrição: Contactos associados a Funcionários (1:1)
-- ========================================================
CREATE TABLE IF NOT EXISTS Contactos_Funcionario (
  ID_Funcionario INT NOT NULL,
  Email VARCHAR(50) UNIQUE,
  Telefone VARCHAR(16) UNIQUE,
  PRIMARY KEY (ID_Funcionario),
  CONSTRAINT fk_ContactosFuncionario_Funcionario
    FOREIGN KEY (ID_Funcionario)
    REFERENCES Funcionario(Id)
);

-- ========================================================
-- Tabela: Veiculo
-- Descrição: Contém os dados dos Veículos para aluguer
-- Inclui restrições para validação de dados
-- ========================================================
CREATE TABLE IF NOT EXISTS Veiculo (
  Matricula VARCHAR(16) NOT NULL,
  NPassageiros INT NOT NULL CHECK (NPassageiros > 0),
  Tara INT NOT NULL,
  CargaMaxima INT NOT NULL,
  Transmissao VARCHAR(1) NOT NULL CHECK (Transmissao IN ('M','A')), -- Manual/Automática
  Combustivel VARCHAR(1) NOT NULL CHECK (Combustivel IN ('G','D','E','H')), -- Gasolina/Diesel/Elétrico/Híbrido
  Consumo DECIMAL(10,2) NOT NULL CHECK (Consumo >= 0),
  Modelo VARCHAR(32) NOT NULL,
  Ano INT NOT NULL,
  Marca VARCHAR(16) NOT NULL,
  Disponivel BOOLEAN NOT NULL DEFAULT FALSE,
  PrecoDiario DECIMAL(10,2) NOT NULL CHECK (PrecoDiario >= 0),
  Tipo VARCHAR(1) CHECK (Tipo IN ('P','M', 'PM')), -- Passageiros, Mercadorias, Ambos
  ID_Stand INT NOT NULL,
  PRIMARY KEY (Matricula),
  CONSTRAINT fk_Veiculo_Stand
    FOREIGN KEY (ID_Stand)
    REFERENCES Stand(Id)
);

-- ========================================================
-- Tabela: Aluguer
-- Descrição: Regista os Alugueres efetuados
-- Cada Aluguer está ligado a um Cliente, Funcionário e Veículo
-- ========================================================
CREATE TABLE IF NOT EXISTS Aluguer (
  Id INT NOT NULL,
  MetodoPagamento VARCHAR(32) NOT NULL,
  Valor DECIMAL(10,2) NOT NULL CHECK (Valor >= 0),
  DataInicio DATE NOT NULL,
  DataFim DATE NOT NULL,
  ID_Cliente INT NOT NULL,
  Matricula VARCHAR(16) NOT NULL,
  ID_Funcionario INT NOT NULL,
  PRIMARY KEY (Id),
  CONSTRAINT fk_Aluguer_Cliente
    FOREIGN KEY (ID_Cliente)
    REFERENCES Cliente(Id),
  CONSTRAINT fk_Aluguer_Veiculo
    FOREIGN KEY (Matricula)
    REFERENCES Veiculo(Matricula),
  CONSTRAINT fk_Aluguer_Funcionario
    FOREIGN KEY (ID_Funcionario)
    REFERENCES Funcionario(Id)
);

-- ===========================
-- Inserção de Dados
-- ===========================

-- Localizacao
INSERT INTO Localizacao (Id, Endereco, CodigoPostal) VALUES
(1, 'Rua Central, 123', '1000-001'),
(2, 'Av. das Laranjeiras, 45', '2000-002');

-- Stand
INSERT INTO Stand (Id, Nome, NIF, ID_Localizacao) VALUES
(1, 'BeloCars Lisboa', 123456789, 1),
(2, 'BeloCars Porto', 987654321, 2);

-- Contactos_Stand
INSERT INTO Contactos_Stand (ID_Stand, Email, Telefone) VALUES
(1, 'lisboa@belocars.pt', '912345678'),
(2, 'porto@belocars.pt', '934567890');

-- Cliente
INSERT INTO Cliente (ID, Nome, DataNascimento, NIF) VALUES
(1, 'João Silva', '1985-06-15', 123456780),
(2, 'Maria Oliveira', '1990-10-20', 234567890);

-- Contactos_Cliente
INSERT INTO Contactos_Cliente (ID_Cliente, Email, Telefone) VALUES
(1, 'joao.silva@email.com', '911223344'),
(2, 'maria.oliveira@email.com', '966778899');

-- Funcionario
INSERT INTO Funcionario (Id, Nome, Departamento, Funcao, ID_Stand) VALUES
(1, 'Carlos Mendes', 'Comercial', 'Vendedor', 1),
(2, 'Ana Costa', 'Administração', 'Gestora', 2);

-- Contactos_Funcionario
INSERT INTO Contactos_Funcionario (ID_Funcionario, Email, Telefone) VALUES
(1, 'carlos.mendes@belocars.pt', '913456789'),
(2, 'ana.costa@belocars.pt', '923456789');

-- Veiculo
INSERT INTO Veiculo (Matricula, NPassageiros, Tara, CargaMaxima, Transmissao, Combustivel, Consumo, Modelo, Ano, Marca, Disponivel, PrecoDiario, Tipo, ID_Stand) VALUES
('00-AA-11', 5, 1200, 500, 'M', 'G', 6.5, 'Civic', 2020, 'Honda', TRUE, 45.00, 'P', 1),
('11-BB-22', 2, 1500, 1000, 'A', 'D', 7.8, 'Sprinter', 2021, 'Mercedes', TRUE, 60.00, 'M', 2);

-- Aluguer
INSERT INTO Aluguer (Id, MetodoPagamento, Valor, DataInicio, DataFim, ID_Cliente, Matricula, ID_Funcionario) VALUES
(1, 'Cartão Crédito', 135.00, '2024-05-01', '2024-05-04', 1, '00-AA-11', 1),
(2, 'MB Way', 180.00, '2024-05-10', '2024-05-13', 2, '11-BB-22', 2);
