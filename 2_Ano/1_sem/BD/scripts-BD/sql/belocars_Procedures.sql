-- ========================================================================
-- Base de Dados: BeloCars
-- Descrição: Sistema de Gestão de Aluguer de Veículos - Procedures e Functions
-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
-- Data: Junho de 2025
-- ========================================================================

USE BeloCars;

-- ====================================================================
-- FUNCTIONS
-- Descrição: Funções auxiliares para cálculos e operações de negócio
-- ====================================================================

-- =========================================
-- Função para calcular valor diário de alugueres
-- Parâmetros: f_data (DATE) - Data para calcular o valor
-- Retorna: Valor total dos alugueres que iniciaram na data especificada
-- =========================================
DROP FUNCTION IF EXISTS fn_ValorDiario;
DELIMITER $$
CREATE FUNCTION fn_ValorDiario(
    f_data DATE
)
    RETURNS DECIMAL(10,2)
    DETERMINISTIC
BEGIN
    DECLARE valorDiario DECIMAL(10,2);

    SELECT IFNULL(SUM(Valor), 0) INTO valorDiario
        FROM Aluguer
        WHERE DATE(DataInicio) = f_data;
    RETURN valorDiario;
END $$
DELIMITER ;

-- =========================================
-- Função para calcular valor total de um aluguer
-- Parâmetros: p_Matricula (VARCHAR), p_DataInicio (DATE), p_DataFim (DATE)
-- Retorna: Valor total do aluguer baseado no preço diário e número de dias
-- =========================================
DROP FUNCTION IF EXISTS fn_CalcularValorAluguer;
DELIMITER $$
CREATE FUNCTION fn_CalcularValorAluguer(
    p_Matricula VARCHAR(16),
    p_DataInicio DATE,
    p_DataFim DATE
)
RETURNS DECIMAL(10,2)
DETERMINISTIC
BEGIN
    DECLARE v_PrecoDiario DECIMAL(10,2);
    DECLARE v_NumDias INT;
    DECLARE v_ValorTotal DECIMAL(10,2);

    SELECT PrecoDiario INTO v_PrecoDiario
    FROM Veiculo
    WHERE Matricula = p_Matricula;

    SET v_NumDias = DATEDIFF(p_DataFim, p_DataInicio) + 1;
    SET v_ValorTotal = v_NumDias * v_PrecoDiario;

    RETURN v_ValorTotal;
END $$
DELIMITER ;

-- ====================================================================
-- PROCEDURES DE FILTROS E CONSULTAS
-- Descrição: Procedures para consulta e filtros de dados
-- ====================================================================

-- =========================================
-- Procedure para filtrar veículos por múltiplos critérios
-- Parâmetros: Todos opcionais (NULL ignora o filtro)
-- Retorna: Lista de veículos que atendem aos critérios
-- =========================================
DROP PROCEDURE IF EXISTS filtraVeiculos;
DELIMITER $$
CREATE PROCEDURE filtraVeiculos(
    IN p_Marca VARCHAR(16),
    IN p_Modelo VARCHAR(32),
    IN p_AnoFabrico INT,
    IN p_PrecoDiario DECIMAL(10,2),
    IN p_Consumo DECIMAL(10,2),
    IN p_NumPassageiros INT,
    IN p_TipoCombustivel VARCHAR(1),
    IN p_TipoVeiculo VARCHAR(2),
    IN p_Disponivel BOOLEAN,
    IN p_Stand INT 
)
BEGIN
    SELECT *
    FROM vwVeiculosGeral
    WHERE (p_Marca IS NULL OR Marca = p_Marca)
      AND (p_Modelo IS NULL OR Modelo = p_Modelo)
      AND (p_AnoFabrico IS NULL OR Ano = p_AnoFabrico)
      AND (p_PrecoDiario IS NULL OR PrecoDiario = p_PrecoDiario)
      AND (p_Consumo IS NULL OR Consumo = p_Consumo)
      AND (p_NumPassageiros IS NULL OR NPassageiros = p_NumPassageiros)
      AND (p_TipoCombustivel IS NULL OR Combustivel = p_TipoCombustivel)
      AND (p_TipoVeiculo IS NULL OR Tipo = p_TipoVeiculo)
      AND (p_Disponivel IS NULL OR Disponivel = p_Disponivel)
      AND (p_Stand IS NULL OR ID_Stand = p_Stand);
END $$
DELIMITER ;

-- =========================================
-- Procedure para filtrar funcionários por departamento e função
-- Parâmetros: p_Departamento, p_Funcao (opcionais)
-- Retorna: Lista de funcionários que atendem aos critérios
-- =========================================
DROP PROCEDURE IF EXISTS filtraFuncionarios;
DELIMITER $$
CREATE PROCEDURE filtraFuncionarios(
    IN p_Departamento VARCHAR(50),
    IN p_Funcao VARCHAR(50)
)
BEGIN 
    SELECT * 
    FROM vwFuncionariosGeral
    WHERE (p_Departamento IS NULL OR Departamento = p_Departamento)
    AND   (p_Funcao IS NULL OR Funcao = p_Funcao);
END $$
DELIMITER ;

-- =========================================
-- Procedure para filtrar alugueres por critérios diversos
-- Parâmetros: Datas, valores e método de pagamento (opcionais)
-- Retorna: Lista de alugueres que atendem aos critérios
-- =========================================
DROP PROCEDURE IF EXISTS filtraAlugueres;
DELIMITER $$
CREATE PROCEDURE filtraAlugueres(
    IN p_DataInicio DATE,
    IN p_DataFim DATE,
    IN p_ValorMin DECIMAL(10,2),
    IN p_ValorMax DECIMAL(10,2),
    IN p_MetodoPagamento VARCHAR(32)
)
BEGIN
    SELECT *
    FROM vwAlugueresGeral
    WHERE (p_DataInicio IS NULL OR DataInicio >= p_DataInicio)
      AND (p_DataFim IS NULL OR DataFim <= p_DataFim)
      AND (p_ValorMin IS NULL OR Valor >= p_ValorMin)
      AND (p_ValorMax IS NULL OR Valor <= p_ValorMax)
      AND (p_MetodoPagamento IS NULL OR MetodoPagamento = p_MetodoPagamento);
END $$
DELIMITER ;

-- ====================================================================
-- PROCEDURE: sp3TopClientesStand
-- Descrição: Retorna os 3 clientes que mais gastaram num stand específico
-- Parâmetros: NrStand (INT) - ID do stand a consultar
-- Retorna: Stand, Nome do Cliente e Valor Total Pago
-- ====================================================================
DROP PROCEDURE IF EXISTS sp3TopClientesStand;
DELIMITER $$
CREATE PROCEDURE sp3TopClientesStand 
    (IN NrStand INT)
BEGIN
    SELECT S.Id AS "Stand", 
           S.Nome AS "Nome Stand",
           C.Nome AS "Cliente", 
           SUM(A.Valor) AS "Valor Total Pago" 
    FROM Aluguer AS A 
        INNER JOIN Cliente AS C
            ON A.ID_Cliente = C.ID
        INNER JOIN Funcionario AS F
            ON F.Id = A.ID_Funcionario
        INNER JOIN Stand AS S
            ON F.ID_Stand = S.Id
    WHERE S.Id = NrStand
    GROUP BY C.ID, C.Nome, S.Id, S.Nome
    ORDER BY SUM(A.Valor) DESC
    LIMIT 3;
END $$
DELIMITER ;

-- =========================================
-- Procedure para listar alugueres de um cliente específico
-- Parâmetros: p_Cliente (obrigatório), datas (opcionais)
-- Retorna: Histórico de alugueres do cliente
-- =========================================
DROP PROCEDURE IF EXISTS listaAlgueresCliente;
DELIMITER $$
CREATE PROCEDURE listaAlgueresCliente(
    IN p_Cliente INT,
    IN p_DataInicio DATE,
    IN p_DataFim DATE
)
BEGIN
    SELECT VWAC.Matricula, VWAC.DataInicio, VWAC.DataFim,
    VWAC.Marca, VWAC.Modelo 
    FROM vwAlugueresCliente AS VWAC
    JOIN Cliente AS C ON C.ID = VWAC.ID_Cliente
    WHERE (p_Cliente = VWAC.ID_Cliente)
    AND (p_DataInicio IS NULL OR VWAC.DataInicio >= p_DataInicio)
    AND (p_DataFim IS NULL OR VWAC.DataFim <= p_DataFim);
END $$
DELIMITER ;

-- =========================================
-- Procedure para listar alugueres processados por um funcionário
-- Parâmetros: p_Funcionario (obrigatório), datas (opcionais)
-- Retorna: Lista de alugueres processados pelo funcionário
-- =========================================
DROP PROCEDURE IF EXISTS listaAlugueresFuncionario;
DELIMITER $$
CREATE PROCEDURE listaAlugueresFuncionario(
    IN p_Funcionario INT,
    IN p_DataInicio DATE,
    IN p_DataFim DATE
)
BEGIN
    SELECT VWAF.Matricula, VWAF.DataInicio,
    VWAF.DataFim, VWAF.Valor FROM vwAlugueresFuncionario AS VWAF
    JOIN Funcionario AS F ON VWAF.ID_Funcionario = F.Id
    WHERE (VWAF.ID_Funcionario = p_Funcionario)
    AND (p_DataInicio IS NULL OR VWAF.DataInicio >= p_DataInicio)
    AND (p_DataFim IS NULL OR VWAF.DataFim <= p_DataFim);
END $$
DELIMITER ;

-- ====================================================================
-- PROCEDURES DE GESTÃO DE CLIENTES
-- Descrição: Operações CRUD para entidade Cliente
-- ====================================================================

-- =========================================
-- Procedure para adicionar um novo cliente
-- Parâmetros: Dados do cliente (ID, Nome, DataNascimento, NIF)
-- Validações: ID único, NIF único, data de nascimento válida
-- =========================================
DROP PROCEDURE IF EXISTS spAdicionarCliente;
DELIMITER $$
CREATE PROCEDURE spAdicionarCliente(
    IN p_ID INT,
    IN p_Nome VARCHAR(50),
    IN p_DataNascimento DATE,
    IN p_NIF INT
)
BEGIN
    IF EXISTS (SELECT 1 FROM Cliente WHERE ID = p_ID) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Já existe um cliente com esse ID.';
    END IF;

    IF p_NIF IS NOT NULL AND EXISTS (SELECT 1 FROM Cliente WHERE NIF = p_NIF) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Já existe um cliente com esse NIF.';
    END IF;

    IF p_DataNascimento > CURRENT_DATE THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Data de nascimento inválida.';
    END IF;

    INSERT INTO Cliente (ID, Nome, DataNascimento, NIF)
    VALUES (p_ID, p_Nome, p_DataNascimento, p_NIF);
END $$
DELIMITER ;

-- =========================================
-- Procedure para remover um cliente
-- Parâmetros: p_ID (obrigatório)
-- Nota: Alugueres são removidos automaticamente por CASCADE
-- =========================================
DROP PROCEDURE IF EXISTS spApagarCliente;
DELIMITER $$
CREATE PROCEDURE spApagarCliente(
    IN p_ID INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Cliente WHERE ID = p_ID) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Cliente não existe.';
    END IF;

    DELETE FROM Cliente WHERE ID = p_ID;
END $$
DELIMITER ;

-- ====================================================================
-- PROCEDURES DE GESTÃO DE FUNCIONÁRIOS
-- Descrição: Operações CRUD para entidade Funcionário
-- ====================================================================

-- =========================================
-- Procedure para adicionar um novo funcionário
-- Parâmetros: Dados do funcionário incluindo ID do stand
-- Validações: Stand deve existir
-- =========================================
DROP PROCEDURE IF EXISTS spAdicionarFuncionario;
DELIMITER $$
CREATE PROCEDURE spAdicionarFuncionario(
    IN p_Id INT,
    IN p_Nome VARCHAR(50),
    IN p_Departamento VARCHAR(50),
    IN p_Funcao VARCHAR(50),
    IN p_ID_Stand INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Stand WHERE Id = p_ID_Stand) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: O stand especificado não existe.';
    END IF;

    INSERT INTO Funcionario (
        Id, Nome, Departamento, Funcao, ID_Stand
    ) VALUES (
        p_Id, p_Nome, p_Departamento, p_Funcao, p_ID_Stand
    );
END $$
DELIMITER ;

-- =========================================
-- Procedure para remover um funcionário
-- Parâmetros: p_Id (obrigatório)
-- Validações: Funcionário deve existir
-- =========================================
DROP PROCEDURE IF EXISTS spRemoverFuncionario;
DELIMITER $$
CREATE PROCEDURE spRemoverFuncionario(
    IN p_Id INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Funcionario WHERE Id = p_Id) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: O funcionário especificado não existe.';
    ELSE
        DELETE FROM Funcionario WHERE Id = p_Id;
    END IF;
END $$
DELIMITER ;

-- ====================================================================
-- PROCEDURES DE GESTÃO DE VEÍCULOS
-- Descrição: Operações CRUD para entidade Veículo
-- ====================================================================

-- =========================================
-- Procedure para adicionar um novo veículo
-- Parâmetros: Todos os dados do veículo
-- Validações: Tipo de veículo (P, M, PM) com regras específicas
-- =========================================
DROP PROCEDURE IF EXISTS spAdicionarVeiculo;
DELIMITER $$
CREATE PROCEDURE spAdicionarVeiculo(
    IN p_Matricula VARCHAR(16),
    IN p_NPassageiros INT,
    IN p_Tara INT,
    IN p_CargaMaxima INT,
    IN p_Transmissao VARCHAR(1),
    IN p_Combustivel VARCHAR(1),
    IN p_Consumo DECIMAL(10,2),
    IN p_Modelo VARCHAR(32),
    IN p_Ano INT,
    IN p_Marca VARCHAR(16),
    IN p_Disponivel BOOLEAN,
    IN p_PrecoDiario DECIMAL(10,2),
    IN p_Tipo VARCHAR(2),
    IN p_ID_Stand INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Stand WHERE Id = p_ID_Stand) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: O stand especificado não existe.';
    END IF;

    IF p_Tipo = 'P' THEN
        IF p_Tara IS NOT NULL OR p_CargaMaxima IS NOT NULL THEN
            SIGNAL SQLSTATE '45000'
            SET MESSAGE_TEXT = 'Erro: Veículo de passageiros não deve ter Tara ou Carga Máxima.';
        END IF;

        IF p_NPassageiros IS NULL THEN
            SIGNAL SQLSTATE '45000'
            SET MESSAGE_TEXT = 'Erro: Veículo de passageiros deve ter número de passageiros válido.';
        END IF;

    ELSEIF p_Tipo = 'M' THEN
        IF p_Tara IS NULL OR p_CargaMaxima IS NULL THEN
            SIGNAL SQLSTATE '45000'
            SET MESSAGE_TEXT = 'Erro: Veículo de mercadorias precisa de Tara e Carga Máxima.';
        END IF;

        IF p_NPassageiros IS NOT NULL THEN
            SIGNAL SQLSTATE '45000'
            SET MESSAGE_TEXT = 'Erro: Veículo de mercadorias não deve ter número de passageiros.';
        END IF;

    ELSEIF p_Tipo = 'PM' THEN
        IF p_Tara IS NULL OR p_CargaMaxima IS NULL THEN
            SIGNAL SQLSTATE '45000'
            SET MESSAGE_TEXT = 'Erro: Veículo misto precisa de Tara e Carga Máxima.';
        END IF;

        IF p_NPassageiros IS NULL THEN
            SIGNAL SQLSTATE '45000'
            SET MESSAGE_TEXT = 'Erro: Veículo misto deve ter número de passageiros válido.';
        END IF;

    ELSE
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Tipo de veículo inválido.';
    END IF;

    INSERT INTO Veiculo (
        Matricula, NPassageiros, Tara, CargaMaxima,
        Transmissao, Combustivel, Consumo, Modelo,
        Ano, Marca, Disponivel, PrecoDiario, Tipo, ID_Stand
    )
    VALUES (
        p_Matricula, p_NPassageiros, p_Tara, p_CargaMaxima,
        p_Transmissao, p_Combustivel, p_Consumo, p_Modelo,
        p_Ano, p_Marca, p_Disponivel, p_PrecoDiario, p_Tipo, p_ID_Stand
    );
END $$
DELIMITER ;

-- =========================================
-- Procedure para remover um veículo
-- Parâmetros: p_Matricula (obrigatório)
-- Validações: Matrícula válida e veículo existente
-- =========================================
DROP PROCEDURE IF EXISTS spRemoverVeiculo;
DELIMITER $$
CREATE PROCEDURE spRemoverVeiculo(
    IN p_Matricula VARCHAR(16)
)
BEGIN
    IF p_Matricula IS NULL OR p_Matricula = '' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Matrícula inválida.';
    END IF;

    IF NOT EXISTS (SELECT 1 FROM Veiculo WHERE Matricula = p_Matricula) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Veículo com essa matrícula não existe.';
    END IF;

    DELETE FROM Veiculo WHERE Matricula = p_Matricula;
END $$
DELIMITER ;

-- ====================================================================
-- PROCEDURES DE GESTÃO DE ALUGUERES
-- Descrição: Operações CRUD para entidade Aluguer
-- ====================================================================

-- =========================================
-- Procedure para adicionar um novo aluguer
-- Parâmetros: Dados completos do aluguer
-- Validações: Datas, existência de cliente, veículo e funcionário
-- =========================================
DROP PROCEDURE IF EXISTS spAdicionarAluguer;
DELIMITER $$
CREATE PROCEDURE spAdicionarAluguer(
    IN p_Id INT,
    IN p_DataInicio DATE,
    IN p_DataFim DATE,
    IN p_ID_Cliente INT,
    IN p_MetodoPagamento VARCHAR(32),
    IN p_Matricula VARCHAR(16),
    IN p_ID_Funcionario INT
)
BEGIN
    IF p_DataInicio >= p_DataFim THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: A data de início deve ser anterior à data de fim.';
    END IF;

    IF NOT EXISTS (SELECT 1 FROM Cliente WHERE Id = p_ID_Cliente) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Cliente não existe.';
    END IF;

    IF NOT EXISTS (SELECT 1 FROM Veiculo WHERE Matricula = p_Matricula) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Veículo não existe.';
    END IF;

    IF NOT EXISTS (SELECT 1 FROM Funcionario WHERE Id = p_ID_Funcionario) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Funcionário não existe.';
    END IF;

    INSERT INTO Aluguer (
        Id, MetodoPagamento, DataInicio, DataFim,
        ID_Cliente, Matricula, ID_Funcionario
    )
    VALUES (
        p_Id,
        p_MetodoPagamento,
        p_DataInicio,
        p_DataFim,
        p_ID_Cliente,
        p_Matricula,
        p_ID_Funcionario
    );
END $$
DELIMITER ;

-- =========================================
-- Procedure para remover um aluguer
-- Parâmetros: p_Id (obrigatório)
-- Validações: Aluguer deve existir
-- =========================================
DROP PROCEDURE IF EXISTS spRemoverAluguer;
DELIMITER $$
CREATE PROCEDURE spRemoverAluguer(
    IN p_Id INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Aluguer WHERE Id = p_Id) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Aluguer não existe.';
    END IF;

    DELETE FROM Aluguer WHERE Id = p_Id;
END $$
DELIMITER ;

-- =========================================
-- Procedure para adicionar método de pagamento a um aluguer
-- Parâmetros: p_IdAluguer, p_MetodoPagamento
-- Validações: Aluguer existente e sem método de pagamento já definido
-- =========================================
DROP PROCEDURE IF EXISTS spAdicionarMetodoPagamento;
DELIMITER $$
CREATE PROCEDURE spAdicionarMetodoPagamento(
    IN p_IdAluguer INT,
    IN p_MetodoPagamento VARCHAR(32)
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Aluguer WHERE Id = p_IdAluguer) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Aluguer não existe.';
    END IF;

    IF EXISTS (
        SELECT 1 FROM Aluguer
        WHERE Id = p_IdAluguer AND MetodoPagamento IS NOT NULL
    ) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Método de pagamento já foi registado.';
    END IF;

    UPDATE Aluguer
    SET MetodoPagamento = p_MetodoPagamento
    WHERE Id = p_IdAluguer;
END $$
DELIMITER ;


-- ====================================================================
-- PROCEDURES DE GESTÃO DE CONTACTOS
-- Descrição: Operações para adicionar contactos às entidades principais
-- ====================================================================

-- ====================================================================
-- Procedure para adicionar contacto a um cliente
-- Parâmetros: ID do cliente, email e telefone
-- Validações: Cliente deve existir, email e telefone únicos
-- ====================================================================
DROP PROCEDURE IF EXISTS spAdicionarContactoCliente;
DELIMITER $$
CREATE PROCEDURE spAdicionarContactoCliente(
    IN p_ID_Cliente INT,
    IN p_Email VARCHAR(50),
    IN p_Telefone VARCHAR(16)
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Cliente WHERE ID = p_ID_Cliente) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Cliente não existe.';
    END IF;

    IF EXISTS (SELECT 1 FROM Contactos_Cliente WHERE ID_Cliente = p_ID_Cliente) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Cliente já tem contactos registados.';
    END IF;

    IF p_Email IS NOT NULL AND EXISTS (SELECT 1 FROM Contactos_Cliente WHERE Email = p_Email) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Email já está em uso por outro cliente.';
    END IF;

    IF p_Telefone IS NOT NULL AND EXISTS (SELECT 1 FROM Contactos_Cliente WHERE Telefone = p_Telefone) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Telefone já está em uso por outro cliente.';
    END IF;

    IF p_Email IS NOT NULL AND p_Email NOT REGEXP '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Formato de email inválido.';
    END IF;

    IF p_Telefone IS NOT NULL AND p_Telefone NOT REGEXP '^[0-9+\\s\\-()]{9,16}$' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Formato de telefone inválido.';
    END IF;

    INSERT INTO Contactos_Cliente (ID_Cliente, Email, Telefone)
    VALUES (p_ID_Cliente, p_Email, p_Telefone);
END $$
DELIMITER ;

-- ====================================================================
-- Procedure para remover contacto de um cliente
-- Parâmetros: ID do cliente
-- Validações: Cliente deve existir e ter contactos registados
-- ====================================================================
DROP PROCEDURE IF EXISTS spRemoverContactoCliente;
DELIMITER $$
CREATE PROCEDURE spRemoverContactoCliente(
    IN p_ID_Cliente INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Contactos_Cliente WHERE ID_Cliente = p_ID_Cliente) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Cliente não tem contactos registados.';
    END IF;

    DELETE FROM Contactos_Cliente WHERE ID_Cliente = p_ID_Cliente;
END $$
DELIMITER ;

-- ====================================================================
-- Procedure para adicionar contacto a um stand
-- Parâmetros: ID do stand, email e telefone
-- Validações: Stand deve existir, email e telefone únicos
-- ====================================================================
DROP PROCEDURE IF EXISTS spAdicionarContactoStand;
DELIMITER $$
CREATE PROCEDURE spAdicionarContactoStand(
    IN p_ID_Stand INT,
    IN p_Email VARCHAR(50),
    IN p_Telefone VARCHAR(16)
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Stand WHERE Id = p_ID_Stand) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Stand não existe.';
    END IF;

    IF EXISTS (SELECT 1 FROM Contactos_Stand WHERE ID_Stand = p_ID_Stand) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Stand já tem contactos registados.';
    END IF;

    IF p_Email IS NOT NULL AND EXISTS (SELECT 1 FROM Contactos_Stand WHERE Email = p_Email) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Email já está em uso por outro stand.';
    END IF;

    IF p_Telefone IS NOT NULL AND EXISTS (SELECT 1 FROM Contactos_Stand WHERE Telefone = p_Telefone) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Telefone já está em uso por outro stand.';
    END IF;

    IF p_Email IS NOT NULL AND p_Email NOT REGEXP '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Formato de email inválido.';
    END IF;

    IF p_Telefone IS NOT NULL AND p_Telefone NOT REGEXP '^[0-9+\\s\\-()]{9,16}$' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Formato de telefone inválido.';
    END IF;

    INSERT INTO Contactos_Stand (ID_Stand, Email, Telefone)
    VALUES (p_ID_Stand, p_Email, p_Telefone);
END $$
DELIMITER ;

-- ====================================================================
-- Procedure para remover contacto de um stand
-- Parâmetros: ID do stand
-- Validações: Stand deve existir e ter contactos registados
-- ====================================================================
DROP PROCEDURE IF EXISTS spRemoverContactoStand;
DELIMITER $$
CREATE PROCEDURE spRemoverContactoStand(
    IN p_ID_Stand INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Contactos_Stand WHERE ID_Stand = p_ID_Stand) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Stand não tem contactos registados.';
    END IF;

    DELETE FROM Contactos_Stand WHERE ID_Stand = p_ID_Stand;

END $$
DELIMITER ;

-- ====================================================================
-- Procedure para adicionar contacto a um funcionário
-- Parâmetros: ID do funcionário, email e telefone
-- Validações: Funcionário deve existir, email e telefone únicos (opcionais)
-- ====================================================================
DROP PROCEDURE IF EXISTS spAdicionarContactoFuncionario;
DELIMITER $$
CREATE PROCEDURE spAdicionarContactoFuncionario(
    IN p_ID_Funcionario INT,
    IN p_Email VARCHAR(50),
    IN p_Telefone VARCHAR(16)
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Funcionario WHERE Id = p_ID_Funcionario) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Funcionário não existe.';
    END IF;

    IF EXISTS (SELECT 1 FROM Contactos_Funcionario WHERE ID_Funcionario = p_ID_Funcionario) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Funcionário já tem contactos registados.';
    END IF;

    IF p_Email IS NOT NULL AND EXISTS (SELECT 1 FROM Contactos_Funcionario WHERE Email = p_Email) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Email já está em uso por outro funcionário.';

    IF p_Telefone IS NOT NULL AND EXISTS (SELECT 1 FROM Contactos_Funcionario WHERE Telefone = p_Telefone) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Telefone já está em uso por outro funcionário.';
    END IF;

    IF p_Email IS NOT NULL AND p_Email NOT REGEXP '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Formato de email inválido.';
    END IF;

    IF p_Telefone IS NOT NULL AND p_Telefone NOT REGEXP '^[0-9+\\s\\-()]{9,16}$' THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Formato de telefone inválido.';
    END IF;

    INSERT INTO Contactos_Funcionario (ID_Funcionario, Email, Telefone)
    VALUES (p_ID_Funcionario, p_Email, p_Telefone);
END $$
DELIMITER ;

-- ====================================================================
-- Procedure para remover contacto de um funcionário
-- Parâmetros: ID do funcionário
-- Validações: Funcionário deve existir e ter contactos registados
-- ====================================================================
DROP PROCEDURE IF EXISTS spRemoverContactoFuncionario;
DELIMITER $$
CREATE PROCEDURE spRemoverContactoFuncionario(
    IN p_ID_Funcionario INT
)
BEGIN
    IF NOT EXISTS (SELECT 1 FROM Contactos_Funcionario WHERE ID_Funcionario = p_ID_Funcionario) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: Funcionário não tem contactos registados.';
    END IF;

    DELETE FROM Contactos_Funcionario WHERE ID_Funcionario = p_ID_Funcionario;
END $$
DELIMITER ;

-- ====================================================================
-- TRIGGERS
-- Descrição: Triggers para validação e automações de negócio
-- ====================================================================

-- =========================================
-- Trigger para verificar conflitos de datas em alugueres
-- Previne que o mesmo veículo seja alugado em períodos sobrepostos
-- =========================================
DROP TRIGGER IF EXISTS trg_VerificarDatasAluguer;
DELIMITER $$
CREATE TRIGGER trg_VerificarDatasAluguer
BEFORE INSERT ON Aluguer
FOR EACH ROW
BEGIN
    IF EXISTS (
        SELECT 1 FROM Aluguer
        WHERE Matricula = NEW.Matricula
          AND (
              (NEW.DataInicio BETWEEN DataInicio AND DataFim) OR
              (NEW.DataFim BETWEEN DataInicio AND DataFim) OR
              (NEW.DataInicio < DataInicio AND NEW.DataFim > DataFim)
          )
    ) THEN
        SIGNAL SQLSTATE '45000'
        SET MESSAGE_TEXT = 'Erro: O veículo já está alugado neste período.';
    END IF;
END $$
DELIMITER ;

-- =========================================
-- Trigger para calcular automaticamente o valor do aluguer
-- Utiliza a função fn_CalcularValorAluguer para definir o valor
-- =========================================
DROP TRIGGER IF EXISTS trg_CalcularValorAluguer;
DELIMITER $$
CREATE TRIGGER trg_CalcularValorAluguer
BEFORE INSERT ON Aluguer
FOR EACH ROW
BEGIN
    SET NEW.Valor = fn_CalcularValorAluguer(NEW.Matricula, NEW.DataInicio, NEW.DataFim);
END $$
DELIMITER ;

-- =========================================
-- Trigger para marcar veículo como indisponível após pagamento
-- Quando método de pagamento é adicionado, o veículo fica indisponível
-- =========================================
DROP TRIGGER IF EXISTS trg_IndisponibilizarVeiculo;
DELIMITER $$
CREATE TRIGGER trg_IndisponibilizarVeiculo
AFTER UPDATE ON Aluguer
FOR EACH ROW
BEGIN
    IF OLD.MetodoPagamento IS NULL AND NEW.MetodoPagamento IS NOT NULL THEN
        UPDATE Veiculo
        SET Disponivel = FALSE
        WHERE Matricula = NEW.Matricula;
    END IF;
END $$
DELIMITER ;

-- ====================================================================
-- EXEMPLOS DE UTILIZAÇÃO
-- Descrição: Exemplos práticos de uso das procedures e functions
-- ====================================================================

/*
-- Filtrar veículos disponíveis de passageiros
CALL filtraVeiculos(NULL, NULL, NULL, NULL, NULL, NULL, NULL, 'P', TRUE, NULL);

-- Consultar os 3 melhores clientes do stand 1
CALL sp3TopClientesStand(1);

-- Adicionar um novo cliente
CALL spAdicionarCliente(1, 'João Silva', '1990-05-15', 123456789);

-- Adicionar um funcionário ao stand 1
CALL spAdicionarFuncionario(1, 'Maria Santos', 'Vendas', 'Vendedor', 1);

-- Adicionar um veículo de passageiros
CALL spAdicionarVeiculo('AA-11-BB', 5, NULL, NULL, 'M', 'G', 7.5, 'Focus', 2020, 'Ford', TRUE, 35.00, 'P', 1);

-- Criar um aluguer
CALL spAdicionarAluguer(1, '2024-01-01', '2024-01-07', 1, 'Cartão de Crédito', 'AA-11-BB', 1);

-- Adicionar contacto a um cliente
CALL spAdicionarContactoCliente(1, 'joao.silva@email.com', '+351 912 345 678');

-- Adicionar contacto a um stand
CALL spAdicionarContactoStand(1, 'geral@belocars-porto.pt', '+351 220 123 456');

-- Adicionar contacto a um funcionário
CALL spAdicionarContactoFuncionario(1, 'maria.santos@belocars.pt', '+351 913 456 789');

-- Consultar alugueres de um cliente
CALL listaAlgueresCliente(1, '2024-01-01', '2024-12-31');

-- Verificar valor diário de alugueres
SELECT fn_ValorDiario('2024-01-01') AS ValorDiario;

-- Calcular valor de um aluguer específico
SELECT fn_CalcularValorAluguer('AA-11-BB', '2024-01-01', '2024-01-07') AS ValorAluguer;
*/