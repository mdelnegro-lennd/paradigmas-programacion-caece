
esVacia :: [a] -> Bool
esVacia [] = True
esVacia lista = False

ultimo :: [a] -> a
ultimo (x:xs) = if esVacia xs then x else ultimo xs

sacarUltimo :: [a] -> [a]
sacarUltimo [x] = []
sacarUltimo (x:xs) = if esVacia xs then [] else x : sacarUltimo xs

capicua :: [Int] -> Bool
capicua [] = True
capicua (x:xs) = if (ultimo xs) == x then capicua (sacarUltimo xs) else False
