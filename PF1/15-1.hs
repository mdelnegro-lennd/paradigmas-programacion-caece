
partes :: [Int] -> [[Int]]
partes [] = [[]]
partes lista = partes2 lista []

partes2 :: [Int] -> [Int] -> [[Int]]
partes2 [] opts = [opts]
partes2 lista opts = partes2 (tail lista) (head lista:opts) ++ partes2 (tail lista) opts

--Llamada Inicial
perm :: [Int] -> [[Int]]
perm lista = permAux lista [] 0

--Inicia recursion
permAux :: [Int] -> [Int] -> Int -> [[Int]]
permAux lista solucion idx 
    | null lista = [solucion]
    | otherwise = permAux2 lista solucion idx

permAux2 :: [Int] -> [Int] -> Int -> [[Int]]
permAux2 lista solucion idx 
    | idx >= length lista = []
    | otherwise = let
        nuevoElemento = (lista !! idx)
        nuevaLista = take idx lista ++ drop (idx + 1) lista
        nuevaSolucion = solucion ++ [nuevoElemento]
        siguientePermutacion = permAux nuevaLista nuevaSolucion 0
        siguienteCiclo = permAux2 lista solucion (idx + 1)
    in siguientePermutacion ++ siguienteCiclo