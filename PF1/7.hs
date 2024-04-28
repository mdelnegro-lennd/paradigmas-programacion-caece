
esVacia :: [a] -> Bool
esVacia [] = True
esVacia lista = False

ultimo :: [a] -> a
ultimo (x:xs) 
    | null xs = x 
    | otherwise = ultimo xs

sacarUltimo :: [a] -> [a]
sacarUltimo [x] = []
sacarUltimo (x:xs) 
    | null xs = [] 
    | otherwise = x : sacarUltimo xs
