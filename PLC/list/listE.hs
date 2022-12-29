import Data.Char()

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result 

changeComma :: String -> String
changeComma [] = []
changeComma (x:xs) | x == ';' = ' ' : changeComma xs
                   | otherwise = x : changeComma xs

stringToArray :: String -> [String]
stringToArray [] = []
stringToArray x = words (changeComma x)

verifyMonth :: [String] -> Int -> [String]
verifyMonth [] count = []
verifyMonth (x:xs) count  | even count && count `mod` 4 /= 0 = x : verifyMonth xs (count + 1)
                          | otherwise = verifyMonth xs (count + 1)

verifyValues :: [String] -> Int -> [Double]
verifyValues [] count = []
verifyValues (x:xs) count | count `mod` 4 == 0 = read x: verifyValues xs (count + 1)
                          | otherwise = verifyValues xs (count + 1)

verifyTuples :: [String] -> [Double] -> String -> [Double]
verifyTuples [] [] month = []
verifyTuples [] ys month = []
verifyTuples xs [] month = []
verifyTuples (x:xs) (y:ys) month | x == month = y : verifyTuples xs ys month 
                                 | otherwise = verifyTuples xs ys month

logMes :: String -> String -> Double
logMes [] [] = 0.0
logMes month xs = foldl (+) 0 (verifyTuples (verifyMonth (stringToArray xs) 1) (verifyValues (stringToArray xs) 1) month)