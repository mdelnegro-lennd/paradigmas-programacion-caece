esPrefija :: Eq a => [a] -> [a] -> Bool
esPrefija [] lista2 = True
esPrefija (x:xs) (y:ys) = if x == y then esPrefija xs ys else False

subcadena :: Int -> Int -> [a] -> [a]
subcadena inicio fin [] = []
subcadena 0 0 (x:xs) = []
subcadena 0 fin (x:xs) = x : subcadena 0 (fin - 1) xs
subcadena inicio fin (x:xs) = subcadena (inicio - 1) (fin - 1) xs

long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

ventana :: Eq a => [a] -> [a] -> Int -> Int -> Int
ventana lista1 lista2 inicio fin = if lista1 == subcadena inicio fin lista2 then inicio else if fin > long lista2 then -1 else ventana lista1 lista2 (inicio + 1) (fin + 1)

posicion :: Eq a => ([a], [a]) -> Int
posicion (lista1, lista2) = ventana lista1 lista2 0 (long lista1)
