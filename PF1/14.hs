izquierda :: Int -> [Int] -> [Int]
izquierda pivote [] = []
izquierda pivote (x:xs) = if x < pivote then x : izquierda pivote xs else izquierda pivote xs

derecha :: Int -> [Int] -> [Int]
derecha pivote [] = []
derecha pivote (x:xs) = if x >= pivote then x : derecha pivote xs else derecha pivote xs

combinar :: [Int] -> [Int] -> [Int]
combinar [] lista2 = lista2
combinar (x:xs) lista2 = x : combinar xs lista2

partir :: [Int] -> [Int]
partir [] = []
partir lista = combinar (partir (izquierda (head lista) lista)) (partir (derecha (head lista) lista))

quicksort :: [Int] -> [Int]
quicksort lista = partir lista