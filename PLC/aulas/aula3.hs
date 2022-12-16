--- Aula 3

sumMine :: [Int] -> Int
sumMine [] = 0
sumMine (b : bs) = b + sumMine bs

double :: [Int] -> [Int]
double [] = []
double (x: xs) = (x * 2) : double xs

member :: [Int] -> Int -> Bool
member [] n = False
member (x: xs) n | n == x = True
                 | otherwise = member xs n

doubleList xs = [2 * a | a <- xs]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs lp = [a+b | (a, b) <- lp]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

-- [x | x <- [1..1000], x > 5, x < 10]
