{-
  1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, e retorna a localização (o índice) daquele elemento dentro da lista. 
  A primeira posição na lista tem índice 0 (zero).
  Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
  Exemplos: locate 'x' "abcdewxyz" ------>  6
            locate 5   [5,98,7,32] ------>  0
            locate True [False, False] --> -1
-}
locate :: Eq t => t -> [t] -> Int
locate c xs = locate' c xs 0
  where locate' x [] z = -1
        locate' x (y:ys) z | x == y = z
                           | otherwise = locate' x ys (z+1)

{-
  2) (3.0) Escreva uma função que verifique se uma lista está contida em outra (por exemplo, se uma String é substring de outra).
  Exemplos: substr "abc" "xyz12abrt" ----> False
            substr "abc" "aaabrsabcfr" --> True
            substr "aab" "aacrtxxeaayb" -> False
-}
substr :: String -> String -> Bool
substr [] _ = True
substr _ [] = False
substr (a:as) (x:xs) | a == x && subAux (a:as) (x:xs) = True
                     | otherwise = substr (a:as) xs

subAux :: String -> String -> Bool
subAux [] _ = True
subAux _ [] = False
subAux (a:as) (x:xs) | a == x = subAux as xs
                     | otherwise = False

{-
  3) Um robô é controlado por 4 comandos: 
    Left, para girar sua direção à esquerda 90 graus;
    Right, para girar sua direção à direita em 90 graus;
    Forward seguido de um número N, que indica um avanço de N metros.
    Backward seguido de um número N, que indica um retrocesso de N metros.

  Supondo que o robô comece na posição (0,0) (coordenadas) e direcionado para norte (i.e. para o posição (0,1)): 
  (3.0) faça uma função destination que informe a localização do robô após uma sequêcia de comandos.

  Exemplo de posições/coordenadas:
  (-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)
  (-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)
  (-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)
  (-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)
  (-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

  exemplo: destination (0,0) [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> (0,1)
          destination (0,0) [Backward 2, Forward 1] ---> (0,-1)
-}

data Command = Forward Int | Backward Int | TurnLeft |  TurnRight 
  deriving (Eq, Show)
data Direction = North | South | West | East
  deriving (Eq, Show)

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination (a,b) [] = (a, b)
destination (a,b) c  = (x,y)
                 where (x,y,z) = foldl go (a, b, North) c

go :: (Int,Int,Direction) -> Command -> (Int, Int, Direction)
go (a, b, c) (Backward d) | c == North = (a, b - d, North)
                          | c == South = (a, b + d, South)
                          | c == East = (a - d, b, East)
                          | c == West = (a + d, b, West)
go (a, b, c) (Forward d)  | c == North = (a, b + d, North)
                          | c == South = (a , b - d, South)
                          | c == East = (a + d, b, East)
                          | c == West = (a - d, b, West)      
go (a, b, c) d = (a, b, turn c d)


{-
4) (2.0) faça uma função faces que informe para qual direção o robô estará voltado ao final de uma sequência de comandos (North, South, East ou West), assumindo que ele começa voltado para a direção North.
exemplo: faces North [Forward 2, TurnLeft, TurnLeft, Forward 1] ---> South
         faces North [Backward 2, Forward 1] ---> North
         faces North [TurnLeft, TurnLeft, TurnLeft] ---> East
-}

faces ::  Direction -> [Command] -> Direction
faces d [] = d
faces d (x:xs) = faces (turn d x) xs 

turn :: Direction -> Command -> Direction
turn x (Forward _) = x
turn x (Backward _) = x
turn North TurnLeft = West
turn North TurnRight = East
turn South TurnLeft = East
turn South TurnRight = West
turn West TurnLeft = South
turn West TurnRight = North
turn East TurnLeft = North
turn East TurnRight = South