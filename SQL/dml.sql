-- Data Manipulation Language (DML)

-- INSERT | UPDATE | DELETE | SELECT

-- INSERT DADOS:
INSERT INTO PESQUISADOR(
  CPF,
  NOME,
  INSTITUICAO,
  NASCIMENTO
) VALUES (
  13458791200,
  'NIELSON',
  'UPFE',
  '01/01/2000'
)

-- Dá para inserir dados a partir de uma consulta:
-- INSERT INTO tabela
-- SELECT CPF, NOME, INSTITUICAO, NASCIMENTO,
-- FROM PESQUISADOR
-- WHERE NASCIMENTO < '01/01/1970';

-- UPDATE ALTERA DADOS! NÃO COLUNAS:

-- Alterar nome e ano do EVENTO especifico
-- UPDATE EVENTO
-- SET NOME = 'NOVO NOME', ANO = 2000
-- WHERE COD = '1111'

-- Alterar todos os ARTIGOS:
-- UPDATE ARTIGO
-- SET NOTA = NOTA - 1;

-- DELETE delete dados! NÃO COLUNAS:
-- DELETE FROM ARTIGO
-- WHERE COD = '1111',

-- DELETE FROM ARTIGO;
-- OBS: Diferente do TRUNCATE remove linhas com uso de condicionais.
-- Pode reverter a operação a partir de logs.