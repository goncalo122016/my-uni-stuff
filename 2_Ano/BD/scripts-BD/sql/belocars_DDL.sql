-- ========================================================================
-- Base de Dados: BeloCars
-- Descrição: Implementação física de um sistema de gestão de Base de Dados
-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
-- Data: Junho de 2025
-- ========================================================================

-- Criação da base de dados (caso não exista)
CREATE DATABASE IF NOT EXISTS BeloCars DEFAULT CHARACTER SET utf8mb4;
USE BeloCars;

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
    ON DELETE CASCADE
    ON UPDATE CASCADE
);

-- ========================================================
-- Tabela: Veiculo
-- Descrição: Contém os dados dos Veículos para aluguer
-- Inclui restrições para validação de dados
-- ========================================================
CREATE TABLE IF NOT EXISTS Veiculo (
  Matricula VARCHAR(16) NOT NULL,
  NPassageiros INT CHECK (NPassageiros > 0),
  Tara INT,
  CargaMaxima INT,
  Transmissao VARCHAR(1) NOT NULL CHECK (Transmissao IN ('M','A')), -- Manual/Automática
  Combustivel VARCHAR(1) NOT NULL CHECK (Combustivel IN ('G','D','E','H')), -- Gasolina/Diesel/Elétrico/Híbrido
  Consumo DECIMAL(10,2) NOT NULL CHECK (Consumo >= 0),
  Modelo VARCHAR(32) NOT NULL,
  Ano INT NOT NULL,
  Marca VARCHAR(16) NOT NULL,
  Disponivel BOOLEAN NOT NULL DEFAULT FALSE,
  PrecoDiario DECIMAL(10,2) NOT NULL CHECK (PrecoDiario >= 0),
  Tipo VARCHAR(2) CHECK (Tipo IN ('P','M', 'PM')), -- Passageiros, Mercadorias, Ambos
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
  MetodoPagamento VARCHAR(32),
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
