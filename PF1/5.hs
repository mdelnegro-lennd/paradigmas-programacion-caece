-- length
long :: [a] -> Int
long [] = 0
long (x:ys) = 1 + (long ys)

-- sum
sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:ys) = x + (sumaLista ys)

-- mem
member :: Int -> [Int] -> Bool
member n [] = False
member n (x:ys) = if x == n then True else member n ys

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
term 0 (x:xs) = x
term n (x:xs) = term (n - 1) xs

-- reverse
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = append (rev xs) (x:[])

-- maximum
max :: Int -> Int -> Int
max n m = if n > m then n else m

-- maximum
maxl :: [Int] -> Int
maxl [] = 0
maxl (x:xs) = Main.max x (maxl xs)

cuenta :: Eq a => a -> [a] -> Int
cuenta el [] = 0
cuenta el lista = if head lista == el then 1 + cuenta el (tail lista) else cuenta el (tail lista)

repite :: Int -> a -> [a]
repite 0 elemento = []
repite n elemento = elemento : repite (n-1) elemento
