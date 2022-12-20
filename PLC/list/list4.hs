parseInput str = let [n, s] = words str
                 in (read n, s)
main :: IO()
main = interact $ uncurry paraDireita . parseInput

addEspacos :: Int -> String
addEspacos n
         | n == 0 = ""
         | otherwise = addEspacos (n-1) ++ " " 

paraDireita :: Int -> String -> String
paraDireita x y = addEspacos x ++ y 