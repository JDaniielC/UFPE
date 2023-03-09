-- José Daniel Silva do Carmo
-- JDSC2

import Data.Char

-- 1) Run Length Encoding é um processo para comprimir caracteres quando existe uma sequência longa de caracteres repetidos. O algoritmo funciona, por exemplo, trocando zeros que se repetem em uma sequência (lista) de números por zero seguido da quantidade de repetiçẽos:

-- Exemplo: rlencode0 [17,8,54,0,0,0,97,5,16,0,45,23,0,0,0,0,0,3,67,0,0,8]
-- Output: [17,8,54,0,3,97,5,16,0,1,45,23,0,5,3,67,0,2,8]

rlencode0 :: [Int] -> [Int]
rlencode0 [] = []
rlencode0 (x:xs) | x == 0 = x : (cutZero xs 0) + 1 : rlencode0 (drop (cutZero xs 0) xs)
                 | otherwise = x: rlencode0 xs

cutZero :: [Int] -> Int -> Int
cutZero [] n = n 
cutZero (x:xs) n | x == 0 = cutZero xs (n+1)
               | otherwise = n

-- Função que descomprime:

-- Exemplo: rldecode0 [17,8,54,0,3,97,5,16,0,1,45,23,0,5,3,67,0,2,8]
-- Output: [17,8,54,0,0,0,97,5,16,0,45,23,0,0,0,0,0,3,67,0,0,8]

rldecode0 :: [Int] -> [Int]
rldecode0 [] = []
rldecode0 [x] = [x]
rldecode0 (x:y:xs) | x == 0 = decodeAux y ++ rldecode0 xs
                   | otherwise = x:rldecode0 (y:xs)

decodeAux :: Int -> [Int]
decodeAux 0 = [] 
decodeAux c = 0: decodeAux (c-1) 

-- 2) Escreva duas funções que fazem esse mesmo processo para qualquer sequência de letras. Assuma que a lista não possui números, apenas letras, e que no máximo as letras se repetem 9 vezes. Se a letra não é seguida por um número é porque ela não se repete naquele momento.


-- Exemplo: rlencodeLetras "abaaacdaaaabbbccxyz"
-- Output: "aba3cda4b3c2xyz"

rlencodeLetras :: String -> String
rlencodeLetras [] = []
rlencodeLetras [x] = [x]
rlencodeLetras (x:[y])  | x == y = x : ['2']
                        | otherwise = x:[y]
rlencodeLetras (x:y:xs) | x == y = [x] ++ show (runLength (x:y:xs)) ++ rlencodeLetras (drop (runLength (x:xs)) (x:xs))
                        | otherwise = x: rlencodeLetras (y:xs)

runLength :: String -> Int
runLength [] = 0
runLength [x] = 1
runLength (x:[y])  | x == y = 2 
runLength (x:y:xs) | x == y = 1 + runLength (y:xs)
                   | otherwise = 1 

-- Descomprimir as letras

-- Exemplo: rldecodeLetras "aba3cda4b3c2xyz"
-- Output: "abaaacdaaaabbbccxyz"

charToInt :: Char -> Int
charToInt ch = read [ch]

rldecodeLetras :: String -> String
rldecodeLetras [] = []
rldecodeLetras [x] = [x]
rldecodeLetras (x:y:xs) | isDigit y = decodeLetrasAux x (charToInt y) ++ rldecodeLetras xs
                        | otherwise = x:rldecodeLetras (y:xs)

decodeLetrasAux :: Char -> Int -> String
decodeLetrasAux _ 0 = [] 
decodeLetrasAux a c = a: decodeLetrasAux a (c-1) 

-- 3) Dado um tipo de dados que representa letras únicas ou letras repetidas:

data Letra = Unica Char
           | Repetida Char Int
        deriving Show

-- Implemente as mesmas funções da questão anterior de forma que a String seja codificada ou decodificada para esse tipo ao invés de diretamente para uma outra String.

-- Exemplo: rlencodeLetrasCodigo "abaaacdaaaabbbccxyz"
-- Output: [Unica 'a',Unica 'b',Repetida 'a' 3,Unica 'c',Unica 'd',Repetida 'a' 4,Repetida 'b' 3,Repetida 'c' 2,Unica 'x',Unica 'y',Unica 'z']

rlencodeLetrasCodigo :: String -> [Letra]
rlencodeLetrasCodigo [] = []
rlencodeLetrasCodigo [x] = [Unica x]
rlencodeLetrasCodigo (x:[y])  | x == y = [Repetida x 2]
                              | otherwise = Unica x : [Unica y]
rlencodeLetrasCodigo (x:y:xs) | x == y = [Repetida x (runLength (x:y:xs))] ++ rlencodeLetrasCodigo (drop (runLength (x:xs)) (x:xs))
                              | otherwise = [Unica x] ++ rlencodeLetrasCodigo (y:xs)

-- Decodificar o tipo

-- Exemplo: rldecodeLetrasCodigo [Unica 'a',Unica 'b',Repetida 'a' 3,Unica 'c',Unica 'd',Repetida 'a' 4,Repetida 'b' 3,Repetida 'c' 2,Unica 'x',Unica 'y',Unica 'z']
-- Output: "abaaacdaaaabbbccxyz"

rldecodeLetrasCodigo :: [Letra] -> String
rldecodeLetrasCodigo [] = []
rldecodeLetrasCodigo ((Unica x):xs) = [x] ++ rldecodeLetrasCodigo xs
rldecodeLetrasCodigo ((Repetida x y):xs) = decodeLetrasAux x y ++ rldecodeLetrasCodigo xs