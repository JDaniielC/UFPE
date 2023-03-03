main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result

mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 (x:xx) [] = (x * 0) : mul2 xx []
mul2 [] (y:xy) = (y * 0) : mul2 [] xy
mul2 (x:xx) (y:xy) = (x * y) : mul2 xx xy
