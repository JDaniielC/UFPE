main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces d [] = d
faces d (x:xs) = faces (turn d x) xs

turn :: Direction -> Command -> Direction
turn North TurnLeft = West
turn North TurnRight = East
turn South TurnLeft = East
turn South TurnRight = West
turn West TurnLeft = South
turn West TurnRight = North
turn East TurnLeft = North
turn East TurnRight = South
turn d e = d -- Comandos de ir para frente ou atrás não faz diferença na questão.

