{-
  (- 1) (2.5) Um dos algoritmos mais simples de compressão de dados sem perda é run-length encoding (RLE),
  em que sequências de dados com o mesmo valor são armazenados como um contador de repetições seguido do
  dado. Escreva uma função encode_rle que, dada uma String, retorna uma lista de pares contendo um caractere
  e o número de vezes que ele se repete de forma seguida na String.

  exemplo: encode_rle "WWWWWWBWWWXYYZZZ" ---> "6W1B3W1X2Y3Z"
-}

encodeRle :: String -> String
encodeRle [] = []
encodeRle (x:xs) = show (runLength (x:xs)) ++ [x] ++ encodeRle (drop (runLength (x:xs)) (x:xs))

runLength :: String -> Int
runLength [] = 0
runLength [x] = 1
runLength [x,y] | x == y = 2
runLength (x:y:xs) | x == y = 1 + runLength (y:xs)
                    | otherwise = 1

-- Pode ser usada uma estrutura de dados intermediaria / auxiliar a critério do aluno.
-- Dica: use a função show para converter um inteiro em String


{-
  (- 2) (2.5) Escreva uma função decode_rle que descomprima uma String codificada com RLE. 

  exemplo: decode_rle "6W1B3W1X2Y3Z" ---> "WWWWWWBWWWXYYZZZ"

  Pode ser usada uma estrutura da dados intermediaria / auxiliar a critério do aluno.
  Assuma que a repetição máxima é de 9 caracteres. 
  Dica: use a função charToInt abaixo para converter um caractere numérico em um inteiro
-}

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

decodeRle :: String -> String
decodeRle [] = []
decodeRle (x:y:xs) = writeString (charToInt x) y ++ decodeRle xs 

writeString :: Int -> Char -> String
writeString 0 b = []
writeString a b = b : writeString (a-1) b

{-
  (- 3) (2.5) Outros algoritmos de compressão utilizam um "dicionário que guarda pares de códigos (inteiro)
  e Strings, de forma que sempre que a String é reutilizada, se usa apenas o código. Para descompactar, é
  preciso ter o dicionário e a String compactada. Ilemente uma função que recebe um dicionário e uma String
  compactada e mostre a String descompactada, isto é, sempre que aparecer um número inteiro, ele deve ser
  substituído pela palavra no dicionário. Para simplificar o problema, assuma que os códigos tem apenas um
  dígito.
-}
type Dicionario = [(Int, String)]
{-
  exemplo: 
  meuDicionario :: Diccionario
  meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]
  teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"
  decode meuDicionario teste ---> "a casa tinha cafe mas nao era uma cafeteria, a casa era uma sorveteria"
-}
decode :: Dicionario -> String -> String
decode dic [] = []
decode dic (x:xs) = translateValue dic x ++ decode dic xs

translateValue :: Dicionario -> Char -> String
translateValue [] c = [c]
translateValue ((a, b): xs) c | a == charToInt c = b
                              | otherwise = translateValue xs c

{-
(- 4) (2.5) Ao invés de uma busca linear no dicionário representado como uma lista, ele pode ser representado
por uma árvore binária, acelerando a busca. Implemente a função codeTree abaixo que, ao invés de receber o
dicionário como uma lista, o recebe como uma árvore de busca binária.
-}

type DicionarioT = Tree Int String
data Tree chave valor = Node chave valor (Tree chave valor) (Tree chave valor)
                      | Leaf

meuDicionarioT :: DicionarioT
meuDicionarioT = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf)
                                (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))

-- decodeTree meuDicionarioT teste ---> "a casa tinha cafe mas nao era uma cafeteria, a casa era uma sorveteria"
decodeTree :: DicionarioT -> String -> String
decodeTree dic [] = []
decodeTree dic (x:xs) = mapTree dic x ++ decodeTree dic xs

mapTree :: DicionarioT -> Char -> String
mapTree Leaf c = [c]
mapTree (Node chave valor esq dir) c | chave == charToInt c = valor
                                     | chave > charToInt c = mapTree esq c
                                     | otherwise = mapTree dir c