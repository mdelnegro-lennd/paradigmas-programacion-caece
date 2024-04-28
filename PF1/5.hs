-- length
long :: [a] -> Int
long [] = 0
long (_:ys) = 1 + (long ys)

-- sum
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:ys) = x + (sumaLista ys)

-- mem
member :: Int -> [Int] -> Bool
member n [] = False
member n (x:ys) 
    | x == n = True 
    | otherwise = member n ys

-- ++
append :: [a] -> [a] -> [a]
append [] lista2 = lista2
append (x:xs) lista2 = x : append xs lista2

-- take
tomar :: Int -> [a] -> [a]
tomar 0 lista = []
tomar n (x:xs) = x : tomar (n-1) xs

-- !!
term :: Int -> [a] -> a
term 0 (x:_) = x
term n (_:xs) = term (n - 1) xs

-- reverse
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = append (rev xs) [x]

-- maximum
max :: Int -> Int -> Int
max n m = if n > m then n else m

-- maximum
maxl :: [Int] -> Int
maxl [] = 0
maxl (x:xs) = Main.max x (maxl xs)

cuenta :: Eq a => a -> [a] -> Int
cuenta el [] = 0
cuenta el lista 
    | head lista == el = 
        let 
            nuevaLista = tail lista
            siguienteCuenta = cuenta el nuevaLista
        in 1 + siguienteCuenta
    | otherwise = 
        let 
            nuevaLista = tail lista 
            siguienteCuenta = cuenta el nuevaLista
        in siguienteCuenta

repite :: Int -> a -> [a]
repite 0 elemento = []
repite n elemento = 
    let 
        siguienteRepeticion = repite (n-1) elemento
    in elemento : siguienteRepeticion

