primesAux:: Int-> Int-> Bool
primesAux 1 n = False
primesAux 2 n = True
primesAux a 1 = True 
primesAux a b |a `mod` b /= 0 = primesAux a (b-1)
           |otherwise = False

primes::Int->Bool
primes a = primesAux a (a-1)

decomp :: Int -> Int -> [Int]
decomp x y | (x + 1) == y = []
decomp x y | x `mod` y == 0 && primes y = y : decomp x (y + 1)
           | otherwise = decomp x (y + 1)

verifyDiv :: Int -> Int -> Int
verifyDiv x y | x `mod` y == 0 = 1 + verifyDiv (x `div` y) y
              | otherwise = 0

transformTuple :: Int -> [Int] -> [(Int, Int)]
transformTuple y [] = []
transformTuple 1 xs = []
transformTuple y (x:xs) | y == x = [(y, 1)]
transformTuple y (x:xs) = (x, verifyDiv y x) : transformTuple y xs

fatPrime :: Int -> [(Int, Int)]
fatPrime x = transformTuple x (decomp x 2)

main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result
