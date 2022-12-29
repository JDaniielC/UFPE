import Data.Char

main = do
    a <- getLine
    let result = minMaxCartao a
    print result

changeComma :: String -> String
changeComma [] = []
changeComma (x:xs) | x == ';' = ' ' : changeComma xs
                   | otherwise = x : changeComma xs

stringToArray :: String -> [String]
stringToArray [] = []
stringToArray x = words (changeComma x)

verifyValues :: [String] -> Int -> [Double]
verifyValues [] count = []
verifyValues (x:xs) count | count `mod` 4 == 0 = read x: verifyValues xs (count + 1)
                          | otherwise = verifyValues xs (count + 1)

minMaxCartao :: String -> (Double, Double)
minMaxCartao [] = (0, 0)
minMaxCartao str = (minimum (verifyValues (stringToArray str) 1), maximum (verifyValues (stringToArray str) 1))