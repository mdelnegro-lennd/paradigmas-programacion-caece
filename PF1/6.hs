generarLista :: Int -> Int -> [Int]
generarLista n1 n2 = n1 : (if n1 == n2 then [] else generarLista (n1 + 1) n2)

a <-> b = generarLista a b

multiplicar :: [Int] -> Int
multiplicar [] = 1
multiplicar (x:xs) = x * multiplicar xs

factorial :: Int -> Int
factorial n = multiplicar (1 <-> n)
