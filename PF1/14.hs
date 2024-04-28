izquierda :: Int -> [Int] -> [Int]
izquierda pivote [] = []
izquierda pivote (x:xs) = if x < pivote then x : izquierda pivote xs else izquierda pivote xs

derecha :: Int -> [Int] -> [Int]
derecha pivote [] = []
derecha pivote (x:xs) = if x > pivote then x : derecha pivote xs else derecha pivote xs

combinar :: [Int] -> Int -> [Int] -> [Int]
combinar [] pivote [] = [pivote]
combinar [] pivote lista2 = pivote : lista2
combinar (x:xs) pivote lista2 = x : combinar xs pivote lista2

pivotar :: [Int] -> Int
pivotar lista = head lista

partir :: [Int] -> [Int]
partir [] = []
partir lista = combinar (partir (izquierda (pivotar lista) lista)) (pivotar lista) (partir (derecha (pivotar lista) lista))

quicksort :: [Int] -> [Int]
quicksort lista = partir lista