 {-
 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
   do menor para o maior elemento..
   exemplo:    ------> True
            isSorted [1,6,8,7,9] ------> False
   Dica: verifique se sua resposta funciona para listas de tamanho ímpar.
-}
isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:[y])  = x <= y
isSorted (x:y:xs) | x <= y = isSorted (y:xs)
                  | otherwise = False

{-}
2) (2.5) O método de ordenação bubble-sort funciona da seguinte forma: 
   cada elemento da lista de entrada é comparado com o seguinte, 
   e se eles não estiverem em ordem (do menor para o maior) sua posição na lista resultante é trocada,
   e a comparação continua com a nova ordem.Esse processo é repetido até que a lista esteja ordenada 
   (nenhuma troca seja mais necessária).
   exemplo, passo a passo: 
       bSort [4,8,3,6,1,8] ----> compara 4 e 8, 8 e 3 (troca, pois 8 > 3), 8 e 6(troca novamente), 8 e 1 (troca novamente) e 8 e 8  
                                   ----> [4,3,6,1,8,8]
       repetindo o processo, temos  ---> [3,4,1,6,8,8] ---> [3,1,4,6,8,8]  ---> [1,3,4,6,8,8]
Implemente a função bSort.
Dica 1: use funções auxiliares, que façam parte do processo;
Dica 2: verifique que sua solução funciona para listas de tamanho ímpar.
-}
bSort :: Ord t => [t] -> [t]
bSort [] = []
bSort [x] = [x]
bSort (x:xs) | isSorted (x:xs) = x:xs
             | otherwise = bSort (sort (x:xs))

sort :: Ord t => [t] -> [t]
sort [] = []
sort (x:[y]) | x < y = x : [y]
              | otherwise = y : [x]
sort (x:y:xs) | x < y = x : sort (y:xs)
              | otherwise = y : sort (x:xs)


{-}
4) (2.5) Dada o tipo de dados Tree t, abaixo, que reresenta uma árvore binária 
com informações (valores) em seus nós, faça uma função isSortedTree que informa se uma árvore está ordenada, ou seja, os valores em nós ou folhas na sub-àrvore à esquerda são sempre menores ou iguais ao valor do nó, e os da sub-árvore à direita sempre maiores ou iguais.
isSortedTree testeOrdenado ----> True
isSortedTree testeNaoOrdenado ----> False
-}
data Tree t = Node t (Tree t) (Tree t) 
            | Leaf t

testeOrdenado :: Tree Int
testeOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 14) (Leaf 17))
testeNaoOrdenado :: Tree Int
testeNaoOrdenado = Node 10 (Node 5 (Leaf 3) (Leaf 6)) (Node 15 (Leaf 16) (Leaf 17))



isSortedTree :: Ord t => Tree t -> Bool
isSortedTree (Leaf y) = True
isSortedTree (Node x esq dir) | (x > (theValue esq)) && (x < (theValue dir)) = (isSortedTree dir) && (isSortedTree esq)
                              | otherwise = False


theValue :: Ord t => Tree t -> t
theValue (Leaf x) = x
theValue (Node x y z) = x  