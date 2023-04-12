{- 
  1) (2.0) Considerando uma chave para representar esta cifra como uma lista de pares
  informando que caracter deve substituir qual outro, implemente uma função cipher para
  realizar esta substituição em uma String, a partir de uma Chave, retornando uma String
  modificada como resultado.
   Qualquer caracter que não esteja na Chave deve aparecer inalterado na String de saída.
-}

type Chave = [(Char, Char)]

-- rot13parcial :: Chave -- troca 'a' por 'n', 'b' por 'o' etc.
rot13parcial :: Chave
rot13parcial =  [('a','n'),('b','o'),('c','p'),('d','q'),('e','r'),('f','s'),
                 ('g','t'),('h','u'),('i','v'),('j','w'),('k','x'),('l','y'), ('m','z')]

changeLetter :: Chave -> Char -> Char
changeLetter [] a = a
changeLetter ((x, b) :xs) a | x == a = b
                            | otherwise = changeLetter xs a

-- exemplo: cipher rot13parcial "hello*hello" ----> "uryyo*uryyo"
cipher :: Chave -> String -> String
cipher [] [] = []
cipher x [] = []
cipher as (x:xs) = changeLetter as x: cipher as xs

{- 
  2) (2.0) Para decifrar o código é preciso inverter a Chave. Faça uma função que
  inverta a ordem de cada um dos pares de uma Chave (lista de pares de caracteres):
  exemplo: inverteChave rot13parcial -----> [('n',’a’),('o',’b’),(’p’,’c’),...
-}

inverteChave :: Chave -> Chave
inverteChave [] = []
inverteChave ((a, b): xs) = (b, a) : inverteChave xs

{- 
  3) (2.0) Uma implementação diferente da função cipher poderia usar uma função no lugar
  dos pares de caracteres para representar a chave.
  
  Defina esta outra versão da função cipher, chamada cipherf
-}

type FuncaoChave = (Char -> Char)

trocaSoLetraL :: FuncaoChave
trocaSoLetraL 'l' = 'b'
trocaSoLetraL c = c

-- exemplo: cipherf trocaSoLetraL "hello*hello" ----> "hebbo*hebbo"
cipherf :: FuncaoChave -> String -> String
cipherf f [] = []
cipherf f xs = map f xs

{- 
  4) (2,0) usando uma expressão do tipo Chave (lista de pares), você conseguiria gerar
  uma função que pode ser usada como chave para a função cipherf?
 Em caso afirmativo, demonstre. Caso contrário, justifique.
-}

-- exemplo: cipherf (chaveToFuncaoChave rot13parcial) "hello*hello" ----> "uryyo*uryyo"
chaveToFuncaoChave :: Chave -> FuncaoChave
chaveToFuncaoChave chave c = case filter (\tup -> fst tup == c) chave of
                            [] -> c
                            xs -> snd (head xs)

chaveTFuncaoChave :: Chave -> FuncaoChave
chaveTFuncaoChave chave c = case lookup c chave of
                            Just x -> x
                            Nothing -> c

chaveTFChave :: Chave -> FuncaoChave
chaveTFChave chave c = maybe c id (lookup c chave)

-- Perceba que FuncaoChave = (Char -> Char) e changeLetter Char -> Char
cToFC :: Chave -> FuncaoChave
cToFC chave = changeLetter chave

{- 
  5) (2,0) Para uma maior eficiência no processo de busca dos caracteres em uma chave,
  podemos fazer a busca dos caracteres a serem trocados usando uma árvore binária em que
  cada nó, além da informação de uma letra e sua substituição, possua sub-árvores para a
  busca de letras menores ou maiores que aquela letra. Se a letra não for achada na árvore
  a substituição não ocorre. Escreva esta função cipherT, que usa uma árvore:
-}
data KeyTree = Node Char Char KeyTree KeyTree | Empty

chaveParcial :: KeyTree
chaveParcial = Node 'h' 'u' 
               (Node 'c' 'p' 
                (Node 'b' 'o' 
                 (Node 'a' 'n' Empty Empty) 
                  Empty)
                (Node 'e' 'r' Empty Empty))
               (Node 'l' 'y' Empty 
                (Node 'm' 'z' Empty Empty))

-- exemplo: cipherT chaveParcial “hello*hello” -----> "uryyo*uryyo"
cipherT :: KeyTree -> String -> String
cipherT keytree (x:xs) = map (findLetter keytree) xs

findLetter :: KeyTree -> Char -> Char
findLetter Empty c = c
findLetter (Node a b as bs) x | x < a = findLetter as x
                              | x > a = findLetter bs x  
                              | otherwise = b

-- Modo básico:
{- 
cipherT :: KeyTree -> String -> String
cipherT keytree [] = []
cipherT keytree (x: xs) = findLetter keytree x : cipherT keytree xs
-}