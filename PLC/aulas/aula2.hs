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
