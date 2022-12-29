main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result

verifyChar :: Char -> [(Char, Char)] -> Char
verifyChar x [] = x
verifyChar y (x:xs) | y == fst x = snd x
                    | otherwise = verifyChar y xs

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] [] = []
decEnigma [] ys = []
decEnigma (x:xs) ys = verifyChar x ys : decEnigma xs ys