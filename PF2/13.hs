data Matriz = MatrizVacia | Matriz [[Int]] deriving Show

suma :: Matriz -> Matriz -> Matriz
suma MatrizVacia MatrizVacia = MatrizVacia
suma (Matriz lista1) MatrizVacia = MatrizVacia
suma MatrizVacia (Matriz lista2) = MatrizVacia
suma (Matriz []) (Matriz []) = MatrizVacia
suma (Matriz lista1) (Matriz lista2) = Matriz (sumaAux lista1 lista2)

sumaAux :: [[Int]] -> [[Int]] -> [[Int]]
sumaAux [] [] = []
sumaAux (x:xs) (y:ys) = sumaAux2 x y : sumaAux xs ys

sumaAux2 :: [Int] -> [Int] -> [Int]
sumaAux2 [] [] = []
sumaAux2 (x:xs) (y:ys) = (x + y) : sumaAux2 xs ys

transpuesta :: Matriz -> Matriz
transpuesta MatrizVacia = MatrizVacia
transpuesta (Matriz []) = MatrizVacia
transpuesta (Matriz listas) = Matriz (transpuestaAux listas)

transpuestaAux :: [[Int]] -> [[Int]]
transpuestaAux [] = []
transpuestaAux xss@(xs:xss2) = if null xs then transpuestaAux (sacarPrimeraColumaDeListas xss) else transpuestaPrimeraColumnaDeListas xss : transpuestaAux (sacarPrimeraColumaDeListas xss)

transpuestaPrimeraColumnaDeListas :: [[Int]] -> [Int]
transpuestaPrimeraColumnaDeListas [] = []
transpuestaPrimeraColumnaDeListas ([]:xss) = transpuestaPrimeraColumnaDeListas xss
transpuestaPrimeraColumnaDeListas (xs:xss) = if null xs then transpuestaPrimeraColumnaDeListas xss else head xs : transpuestaPrimeraColumnaDeListas xss

sacarPrimeraColumaDeListas :: [[Int]] -> [[Int]]
sacarPrimeraColumaDeListas [] = []
sacarPrimeraColumaDeListas ([]:xss) = sacarPrimeraColumaDeListas xss
sacarPrimeraColumaDeListas (xs:xss) = if null xs then sacarPrimeraColumaDeListas xss else tail xs : sacarPrimeraColumaDeListas xss

producto :: Matriz -> Matriz -> Matriz
producto matriz1 matriz2 = productoAux matriz1 (transpuesta matriz2)

productoAux :: Matriz -> Matriz -> Matriz
productoAux (Matriz lista1) (Matriz lista2) = Matriz (productoFilas lista1 lista2)

productoFilas :: [[Int]] -> [[Int]] -> [[Int]]
productoFilas [] [] = []
productoFilas _ [] = []
productoFilas [] _ = []
productoFilas _ ([]:yss) = []
productoFilas ([]:xss) _ = []
productoFilas (xs:xss) yss = productoFilaPorColumnas xs yss : productoFilas xss yss

productoFilaPorColumnas :: [Int] -> [[Int]] -> [Int]
productoFilaPorColumnas [] _ = []
productoFilaPorColumnas _ [] = []
productoFilaPorColumnas xs (ys: yss) = multiplicarFilePorColumna xs ys ++ productoFilaPorColumnas xs yss

multiplicarFilePorColumna :: [Int] -> [Int] -> [Int]
multiplicarFilePorColumna _ [] = []
multiplicarFilePorColumna (x:xs) (y:ys) = [sumarFila ((x * y) : multiplicarFilePorColumna xs ys)]

sumarFila :: [Int] -> Int
sumarFila [] = 0
sumarFila (x:xs) = x + sumarFila xs

