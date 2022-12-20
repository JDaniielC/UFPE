main :: IO()
main = interact $ show . sumTo . read

sumTo :: Int -> Int
sumTo x | x == 0 = 0
        | otherwise = x + sumTo (x-1)