main = do
    a <- getLine
    let result = executa (read a)
    print result

type Comando = String
type Valor = Int

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa ((c,v):xs) | c == "Divide" && v == 0 = -666
                   | c == "Soma" = v + executa xs
                   | c == "Subtrai" = v - executa xs
                   | c == "Multiplica" = v * executa xs
                   | c == "Divide" = v `div` executa xs
                   | otherwise = 0
                   

