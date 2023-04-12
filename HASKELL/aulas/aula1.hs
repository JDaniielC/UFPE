-- Imports:
import Data.Char

--- Aula 1:
main :: IO ()
main = do putStrLn "Rodando:"

greater :: Int -> Bool
greater y = (y > 71)

square :: Int -> Int
square x = x * x

verify :: Int -> Int -> String
verify x y
           | x == y = "Arrochou"
           | x > y = "Embuchou"
           | otherwise = "Tribruxo"

ver :: Int -> Int -> String
ver x y = if x == y then "Arrochou" else "Tribruxo"

vendas :: Int -> Int
vendas 0 = 50
vendas 1 = 100
vendas 2 = 150
vendas 3 = 200
vendas n = 0

totalVendas :: Int -> Int
totalVendas n
  | n == 0 = vendas 0
  | otherwise = totalVendas (n - 1) + vendas n

maxVendas :: Int -> Int
maxVendas 0 = vendas 0
maxVendas n = max (maxVendas (n-1))
                   (vendas n)

-- Verifica se não houve vendas em uma semana n
vendasNulas :: Int -> Bool
vendasNulas n = (vendas n == 0)

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
