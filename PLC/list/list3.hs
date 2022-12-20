-- Imports:
import Data.Char

main = do
    s <- getLine
    let result = btoi s
    print result

btoi :: String -> Int
btoi [] = 0
btoi (x:xs) = (((ord x) - 48) * (2 ^ length xs)) + btoi xs 