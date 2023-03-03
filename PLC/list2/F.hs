main = do
       a <- getLine
       b <- getLine
       let result = insertList (read a::Tree Int) (read b)
       print result

data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList Nilt [] = Nilt
insertList Nilt (x:xs) = insertList (Node x Nilt Nilt) xs
insertList (Node a left right) [] = Node a left right
insertList (Node a left right) (x:xs) | x < a = insertList (Node a (insertList left [x]) right) xs
                                      | x > a = insertList (Node a left (insertList right [x])) xs
                                      | otherwise = insertList (Node a left right) xs

