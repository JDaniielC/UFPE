--- Aula 4

oneRoot :: Float -> Float -> Float -> Float
oneRoot a b c = -b/(2.0*a)

twoRoots :: Float -> Float -> Float -> (Float, Float)
twoRoots a b c = (d-e, d+e)
  where
  d = -b/(2.0*a)
  e = sqrt(b^2-4.0*a*c)/(2.0*a)

roots :: (Float, Float, Float) -> String
roots (a, b, c)
  | b^2 == 4.0*a*c = show (oneRoot a b c)
  | b^2 > 4.0*a*c = show f ++ " " ++ show s
  | otherwise = "No roots"
    where (f, s) = twoRoots a b c

-- Atividade

type Pessoa = String 
type Livro = String
type BancoDados = [(Pessoa, Livro)]

-- Livros emprestados
baseExemplo :: BancoDados
baseExemplo = [("Sergio", "O senhor dos anéis"),
               ("André", "Duna"),
               ("Fernando", "Strange"),
               ("Fernando", "Game of Thrones")]

-- Escrever as funções:

-- Livros que estão com uma Pessoa
livros :: BancoDados -> Pessoa -> [Livro]
livros [] person = []
livros (el:rest) person | fst el == person = snd el : livros rest person
                        | otherwise = livros rest person

-- Utilizando Gerador e casamento de padrões:
livros2 :: BancoDados -> Pessoa -> [Livro]
livros2 bd person = [liv | (nome, liv2) <- bd, nome == person]

-- Emprestimos de um livro
emprestimos :: BancoDados -> Livro -> [Pessoa]

-- Se  um livro está emprestado
emprestado :: BancoDados -> Livro -> Bool 

-- Quantos livros estão com aquela pessoa
qtdEmprestimos :: BancoDados -> Pessoa -> Int

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados

-- Quick Sort

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x: xs) =
      qSort [y | y <- xs, y < x] ++
      [x] ++
      qSort [y | y <- xs, y >= x]