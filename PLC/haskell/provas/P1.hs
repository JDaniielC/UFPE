{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Graphics.Win32 (HGLOBAL)
jogos1 :: [Jogo]
jogos1 = [(Australia, 1, Dinamarca, 3), (Franca, 2, Peru, 0),
          (Australia, 0, Franca, 2),(Dinamarca, 0, Peru, 0),
          (Dinamarca, 0, Franca, 1), (Australia, 0, Peru, 0),
          (Argentina, 1, Croacia, 0), (Islandia, 0, Nigeria, 1),
          (Argentina, 1, Islandia, 0), (Argentina, 1, Nigeria, 1),
          (Croacia, 0, Islandia, 0), (Croacia, 1, Nigeria, 2)]

{- 
  1) Na copa do mundo de futebol, os grupos contém 4 times, e
  avançam para a fase seguinte os que tem maior número de pontos,
  sendo 3 pontos por uma vitória, 1 por um empate, e zero por uma
  derrota.
  Vamos representar os times e jogos com os tipo de dado e sinonimo
  de tipos abaixo:
-}
data Time = Australia | Dinamarca | Franca | Peru
          | Argentina | Croacia | Islandia | Nigeria
          deriving (Show,Eq)
type Jogo = (Time, Int, Time, Int)

{- 
  Por exemplo: Australia 3 x 1 Dinamarca será representado por
  (Australia, 3, Dinamarca, 1)
  
  a) (2.0) Defina uma função que, dado um time e uma lista de
  jogos, informe quantos gols aquele time fez.
-}
gols :: Time -> [Jogo] -> Int
gols t [] = 0
gols t ((a, x, b, y):xs) | t == a = x + gols t xs
                         | t == b = y + gols t xs
                         | otherwise = gols t xs

{- 
  b) (2.0) Defina uma função que, dado um time e uma lista de
  jogos, qual o seu saldo de gols naquele conjunto de jogos (gols
  feitos - gols tomados).
-}
saldo :: Time -> [Jogo] -> Int
saldo t [] = 0
saldo t ((a, x, b, y):xs) | t == a = x - y + saldo t xs
                          | t == b = y - x + saldo t xs
                          | otherwise = saldo t xs

{- 
  c) (2.0) Defina uma função que, dado um time e uma lista de
  jogos, informe quantos pontos ele obteve naquele conjunto de jogos.
-}
pontos :: Time -> [Jogo] -> Int
pontos t [] = 0
pontos t ((a, x, b, y):xs) | t == a = calcP x y + pontos t xs
                           | t == b = calcP y x + pontos t xs
                           | otherwise = pontos t xs

calcP :: Int -> Int -> Int
calcP x y | x > y = 3
          | x == y = 1
          | otherwise = 0

{- 
  d) (1.0) Defina um tipo de dados para caracterizar um Grupo, que
  contém o nome do grupo (os grupos vão da letra 'A' à Letra 'H') e 4
  times.
-}

data Grupos = Grupo Char Time Time Time Time
   deriving (Show,Eq)

{- 
  e) (3.0) Feito isso, defina uma função que, dado um Grupo e uma
  lista de jogos, retorne o par de times que estão classificados.

  Os classificados são: os dois com maior número de pontos; em
  caso de empate, usa-se o saldo de gols; em caso de continuar empate
  usa-se o número de gols feitos (há regras adicionais, mas vamos
  implementar apenas essas 3).

  exemplos de grupos são: Grupo C: Australia, Dinamarca, Franca,
  Peru;
  Grupo D: Argentina, Croacia, Islandia,
  Nigeria.
-}
{-
type TeamPoint = (Time, Int)

teamPoint :: Time -> [Jogo] -> TeamPoint
teamPoint t games = (t, pontos t games) 

teamPoints :: Grupo -> [Jogo] -> (TeamPoint, TeamPoint, TeamPoint, TeamPoint)
teamPoints (_, a, b, c, d) g = (teamPoint a g, teamPoint b g, teamPoint c g, teamPoint d g)

insere :: Ord t => t -> [t] -> [t]
insere x [] = [x]
insere x (y:ys) | x <= y = x:(y:ys)
                | x >  y = y:insere x ys

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insere x sort xs

minimum ::[Int] -> Int
minimum xs = maximum sort xs

bestTeams :: (TeamPoint, TeamPoint, TeamPoint, TeamPoint) -> (Time, Time)
bestTeams ((a, x), (b, y), (c, w), (d, z)) = 

classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados group games = teamPoints group games  
-}