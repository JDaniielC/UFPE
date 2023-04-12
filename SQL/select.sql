-- Data Manipulation Language (DML)

-- SELECT [DISTINCT | ALL] 
--   { * | [TABELA] COLUNA [AS ALIAS] | [,...] }
-- FROM TABELA [,...TABELA]
-- [ WHERE { Condição|condiçao de sub-consulta } ]
-- [ ORDER BY COLUNA [ASC | DESC],... ]
-- [ GROUP BY COLUNA [HAVING condição ] ]
-- [ { UNION | INTERSECT | EXCEPT } SELECT ... ]

-- SELECT -> PROJEÇÃO
-- FROM -> TABELA OU PRODUTO CARTESIANO DELAS
-- WHERE -> SELEÇÃO

SELECT TITULO, NOTA - 1
FROM ARTIGO;

-- Essa consulta tem 2 colunas por causa do vírgula.
SELECT COUNT (*) AS QUANTIDADE, 'UD' AS UNIDADE
FROM PESQUISADOR;

SELECT ALL IDIOMA
FROM ARTIGO;

SELECT DISTINCT IDIOMA
FROM ARTIGO;

-- Pesquisador ficou com um Alias chamado P
SELECT P.NOME, P.INSTITUICAO
FROM PESQUISADOR P;
-- Alias não prejudica no desempenho da consulta.

SELECT *
FROM ARTIGO
WHERE NOTA > 8;
-- WHERE NOTA IN (6, 7, 9);
-- WHERE NOTA BETWEEN 8 AND 9;
-- WHERE IDIOMA LIKE 'PORTUGUES'
-- WHERE TITULO LIKE '%BANCO DE DADOS%';
-- WHERE A IS NULL; -- <HAVING>

SELECT *
FROM ARTIGO
-- Começar com R
WHERE TITULO LIKE 'R%' AND
  NOTA IS NOT NULL AND
  (IDIOMA = 'PORTUGUES' OR
  IDIOMA = 'INGLES');

SELECT LOWER(P.NOME), UPPER (P.INSTITUICAO)
FROM PESQUISADOR P;

-- Manipulação de string:
-- CONCAT('', '')
-- LENGTH('')
-- REPLACE('ARROCHA', 'AR', '')
-- TRIM('')

-- SELECT 'HELLO WORLD' ORIGINAL, 
--   UPPER(CONCAT('HELLO WORLD', 'TESTE')) MODIFICADO
-- FROM DUAL;

-- Casting:
-- TO_NUMBER('')
-- CAST ('' AS DECIMAL)
-- ROUND (23.143,2)

-- Data do sistema:
-- SELECT SYSDATE

SELECT NOME, NASCIMENTO
FROM PESQUISADOR
ORDER BY NASCIMNEOT DESC, NOME ASC;

-- Junções (Produto cartesiano):
-- INNER JOIN
-- LEFT OUTER JOIN
-- RIGHT OUTER JOIN
-- FULL OUTER JOIN

-- Se utilizar funçao de agregação (AVG), as outras colunas devem entrar como GROUP BY (exemplo o E.SIGLA)
SELECT E.SIGLA, AVG (A.NOTA) AS MEDIA_NOTA
FROM ARTIGO A INNER JOIN
  EVENTO E ON A.COD = E.COD
GROUP BY E.SIGLA;
-- Where filtra antes do agrupamento, having filtra depois do agrupamento.
-- HAVING AVG (A.NOTA) > 8;

-- Função de agregação:
-- MIN
-- COUNT
-- MAX
-- SUM