main = do
    s <- getLine
    let result = evalTree (read s)
    print result

data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node SUM left right) = evalTree left + evalTree right
evalTree (Node MUL left right) = evalTree left * evalTree right
evalTree (Node SUB left right) = evalTree left - evalTree right