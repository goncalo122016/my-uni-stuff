-- ========================================================================
-- Base de Dados: BeloCars
-- Descrição: Definição de Utilizadores, Permissões e Grupos
-- Autores: Gonçalo Castro, Luís Felíco, Afonso Martins e Jorge Barbosa
-- Data: Junho de 2025
-- ========================================================================

USE BeloCars;

-- ========================================================
-- Criar grupos de Utilizadores (por cargo)
-- ========================================================
CREATE ROLE IF NOT EXISTS guest;
CREATE ROLE IF NOT EXISTS staff;
CREATE ROLE IF NOT EXISTS admin;

-- ========================================================
-- Dar permissões por ROLE
-- CONVIDADO pode apenas consultar carros
-- ========================================================
GRANT SELECT ON BeloCars.Carro TO guest;

-- ========================================================
-- FUNCIONÁRIO tem permissões mais alargadas (mas não totais)
-- ========================================================
GRANT SELECT, INSERT, UPDATE, DELETE ON BeloCars.Aluguer TO staff;
GRANT SELECT, UPDATE ON BeloCars.Cliente TO staff;
GRANT SELECT, INSERT, UPDATE ON BeloCars.Carro TO staff;

-- ========================================================
-- ADMIN tem permissões completas
-- ========================================================
GRANT ALL PRIVILEGES ON BeloCars.* TO admin WITH GRANT OPTION;

-- ========================================================
-- Criar utilizadores e associar a ROLES
-- Utilizadores tipo VISITANTE
-- ========================================================
CREATE USER IF NOT EXISTS 'utilizador_guest'@'localhost' IDENTIFIED BY 'guest123';
GRANT guest TO 'utilizador_guest'@'localhost';

-- ========================================================
-- Utilizadores tipo FUNCIONÁRIO
-- ========================================================
CREATE USER IF NOT EXISTS 'func1'@'localhost' IDENTIFIED BY 'func123';
CREATE USER IF NOT EXISTS 'func2'@'localhost' IDENTIFIED BY 'func456';
GRANT staff TO 'func1'@'localhost';
GRANT staff TO 'func2'@'localhost';

-- ========================================================
-- Utilizador tipo ADMINISTRADOR
-- ========================================================
CREATE USER IF NOT EXISTS 'admin_ceo'@'localhost' IDENTIFIED BY 'admin123';
GRANT admin TO 'admin_ceo'@'localhost';






-- ========================================================
-- Atribuir ROLES por defeito
-- ========================================================
SET DEFAULT ROLE ALL TO 
  'utilizador_guest'@'localhost',
  'cliente1'@'localhost', 'cliente2'@'localhost',
  'func1'@'localhost', 'func2'@'localhost',
  'admin_ceo'@'localhost';

-- ========================================================
-- Aplicar as alterações
-- ========================================================
FLUSH PRIVILEGES;