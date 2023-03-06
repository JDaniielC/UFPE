import Data.Char

main :: IO ()
main = do
  input1 <- getLine
  input2 <- getLine
  let result = somarListas (read input1 :: [Int])  (read input2 :: [Int])
  print result

verL :: Int -> Int
verL x = 10 ^ x

toString :: String -> [Int]
toString [] = []
toString (x:ax) = (ord x - 48) : toString(ax)

toNumber :: [Int] -> Int
toNumber [] = 0
toNumber (x:xs) = x * verL (length xs) + toNumber xs

somarListas :: [Int] -> [Int] -> [Int]
somarListas [] [] = []
somarListas x y = toString ( show (toNumber x + toNumber y))
