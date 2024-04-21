long :: [a] -> Int
long lista = length lista

sumaLista :: [Int] -> Int
sumaLista lista = sum lista

member :: Int -> [Int] -> Bool
member n lista = elem n lista

append :: [a] -> [a] -> [a]
append lista1 lista2 = lista1 ++ lista2

tomar :: Int -> [a] -> [a]
tomar n lista = take n lista

term :: Int -> [a] -> a
term n lista = lista !! n

rev :: [a] -> [a]
rev lista = reverse lista

maxl :: [Int] -> Int
maxl lista = maximum lista

cuenta :: Eq a => a -> [a] -> Int
cuenta el [] = 0
cuenta el lista = if head lista == el then 1 + cuenta el (tail lista) else cuenta el (tail lista)

repite :: Int -> a -> [a]
repite n elemento = replicate n elemento

repite2 :: Int -> a -> [a]
repite2 0 elemento = []
repite2 n elemento = elemento : repite2 (n-1) elemento


