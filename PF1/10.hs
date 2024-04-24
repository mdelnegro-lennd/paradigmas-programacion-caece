intercalar :: [[a]] -> [[a]] -> [[a]]
intercalar [] [] = []
intercalar lista1 [] = lista1
intercalar [] lista2 = lista2
intercalar (x:xs) (y:ys) = x : y : intercalar xs ys

aparear :: [Int] -> [Int] -> [Int]
aparear [] [] = []
aparear lista1 [] = lista1
aparear [] lista2 = lista2
aparear (x:xs) (y:ys) = if x > y then  y : x : aparear xs ys else  x : y : aparear xs ys
