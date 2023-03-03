main = do
       a <- getLine
       b <- getLine
       let result = destination (read a) (read b)
       print result

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (x,y) [] = (x,y)
destination (x,y) (Forward n:xs) = destination (x,y+n) xs
destination (x,y) (Backward n:xs) = destination (x,y-n) xs
destination (x,y) (TurnLeft:xs) = destination (y,x) xs
destination (x,y) (TurnRight:xs) = destination (y,x) xs