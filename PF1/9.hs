append :: [a] -> [a] -> [a]
append [] lista2 = lista2
append (x:xs) lista2 = x : append xs lista2

flat :: [[a]] -> [a]
flat [] = []
flat (x:xs) = append (flat xs) x

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

longLI :: [[a]] -> Int
longLI lista = len (flat lista)
