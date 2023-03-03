main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result

bSort :: [String] -> [String]
bSort [] = []
bSort (x: xs) =
      bSort [y | y <- xs, y < x] ++
      [x] ++
      bSort [y | y <- xs, y >= x]