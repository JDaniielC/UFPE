-- Data Definition Language (DDL)

-- OBS:
-- Rodar esse script após criação das tabelas!

-- ALTER TABLE tabela 
-- {ADD [CONSTRAINT] { coluna | restrição }            ||
--  DROP {CONSTRAINT | COLUMN } { coluna | restrição } ||
--  ALTER COLUMN coluna
-- }

-- ADICIONAR O CAMPO EMAIL:
ALTER TABLE PESQUISADOR 
ADD EMAIL VARCHAR(40);

-- OBS: É possível ADD CONSTRAINT a coluna criada.

-- MODIFICAR O CAMPO CPF:
ALTER TABLE PESQUISADOR
ALTER CPF CHAR (11);

-- OBS: CHAR VS VARCHAR
-- CHAR: Tamanho fixo (Telefone, CEP, CPF)
-- Caso coloque menos que o tamanho definido, voltará com espaços em branco
-- VARCHAR: tamanho variado

-- ELIMINAR O CAMPO EMAIL:
ALTER TABLE PESQUISADOR
DROP COLUMN EMAIL;

-- OBS: Para forçar a remoção das restrições de integridade referencial
-- DROP TABLE tabela CASCADE CONSTRAINTS;
-- truncate apaga linhas da tabela sem guardar logs (mais rápido, sem backup)
-- TRUNCATE TABLE MELHORES_ARTIGOS;