-- Imports:
import Data.Char

--- Aula 2:

-- eXor
eXor :: Bool -> Bool -> Bool
eXor x y = (x || y) && not (x && y)

-- Outra forma do eXor:
exor :: Bool -> Bool -> Bool
exor True x = not x
exor False x = x

-- Converter para letra maiscula
offset = ord 'A' - ord 'a'
maiuscula :: Char -> Char
maiuscula ch = chr (ord ch + offset)

-- Função `show` converte qualquer tipo para string.
-- Concatenação: ++, exemplo: "Peixe" ++ "\n" ++ "gato"
-- ceiling, floor, round :: Float -> Int
-- read :: Ftring -> Float
-- Função putStrLn (show 123)
-- Função read?

-- Atividade:

addEspacos :: Int -> String
addEspacos n
         | n == 0 = ""
         | otherwise = addEspacos (n-1) ++ " " 

-- Outro jeito:
-- addEspacos :: Int -> String
-- addEspacos 0 = ""
-- addEspacos n = " " ++ addEspacos (n-1) 

paraDireita :: Int -> String -> String
paraDireita x y = addEspacos x ++ y 

cabecalho :: String
cabecalho = "Semana vendas\n"

imprimeTotal :: Int -> String
imprimeTotal n = "Total" ++ paraDireita 6 (show (totalVendas n)) ++ "\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = paraDireita 3 "0" ++ paraDireita 3 (show (vendas 0))
imprimeSemanas n = imprimeSemanas (n-1) ++
                   paraDireita 2 (show n) ++ paraDireita 3 (show (vendas n)) ++ "\n"

mediaVendas :: Int -> Float
mediaVendas n = fromIntegral (totalVendas n) / fromIntegral (n + 1)

imprimeMedia :: Int -> String
imprimeMedia n = "Média" ++ paraDireita 7 (show (mediaVendas n)) ++ "\n"
 
imprimeTabela :: Int -> IO()
imprimeTabela n = putStr (cabecalho
                          ++ imprimeSemanas n
                          ++ imprimeTotal n
                          ++ imprimeMedia n
                        )

minusOne :: Int -> Int
minusOne n = n - 1

primesAux :: Int -> Int -> Bool
primesAux n x
  | x == 1 = True
  | n `mod` x == 0 = False
  | otherwise = primesAux n (x -1)

primes :: Int -> String
primes n
  | n == 0 = show False
  | n == 1 = show False
  | otherwise = show (primesAux n (n - 1))
