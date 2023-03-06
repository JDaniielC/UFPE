main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result

data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node a left right) = isBST left && isBST right && isOrdered left a && isOrdered right a

isOrdered :: Ord t => Tree t -> t -> Bool
isOrdered Nilt _ = True
isOrdered (Node a left right) x | a < x = isOrdered left a && isOrdered right x
                                | a > x = isOrdered left x && isOrdered right a
                                | otherwise = False

