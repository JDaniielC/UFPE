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
