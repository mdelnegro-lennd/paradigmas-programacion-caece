permutar :: [Int] -> [[Int]]
permutar [] = []                             --Caso Base cuando no hay elementos
permutar [x] = [[x]]                         --Caso Base cuando hay 1 solo elemento
permutar (x:xs) = intercalar x (permutar xs) --Caso recursivo: Intercalar la cabeza con todas las permutaciones

intercalar :: Int -> [[Int]] -> [[Int]]
intercalar _ [] = []
intercalar elemento listas@(x:xs) = intercalarEnLista elemento x ++ intercalar elemento xs

intercalarEnLista :: Int -> [Int] -> [[Int]]
intercalarEnLista elemento lista = insertarEn elemento lista 0

insertarEn :: Int -> [Int] -> Int -> [[Int]]
insertarEn elemento lista pos
    | pos > length lista = []
    | otherwise = let 
        nuevaLista = take pos lista ++ [elemento] ++ drop pos lista
        siguientePermutacion = insertarEn elemento lista (pos + 1)
    in nuevaLista : siguientePermutacion