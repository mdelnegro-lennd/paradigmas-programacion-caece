
esVacia :: [a] -> Bool
esVacia [] = True
esVacia lista = False

ultimo :: [a] -> a
ultimo (x:xs) = if esVacia xs then x else ultimo xs

sacarUltimo :: [a] -> [a]
sacarUltimo [x] = []
sacarUltimo (x:xs) 
    | null xs = [] 
    | otherwise = x : sacarUltimo xs

capicua :: [Int] -> Bool
capicua [] = True
capicua (x:xs) 
    | ultimo xs == x = capicua (sacarUltimo xs) 
    | otherwise = False
