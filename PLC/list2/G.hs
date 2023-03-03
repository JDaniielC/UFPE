main = do
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result

data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0 -- Basicamente é somar, só que com a maior altura da árvore.
alturaArvore (Node _ left right) = 1 + max (alturaArvore left) (alturaArvore right)

