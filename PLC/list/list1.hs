main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result

isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 char = True
isReplica [] n char = False
isReplica (x:xs) n char | x == char = isReplica xs (n-1) char
                        | otherwise = False