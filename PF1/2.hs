--i)	Definir la función max, que dados dos números devuelve el máximo de los dos. 
--En Haskell esta función está predefinida para los tipos ordenados y se llama también max.
--ii)	Definir la función max3, que dados tres números devuelve el máximo de los tres. Usar max.
--iii)	Definir la función min, que dados dos números devuelve el mínimo de los dos. 
--En Haskell esta función está predefinida para los tipos ordenados y se llama también min. Usar max.

max :: Int -> Int -> Int
max n m = if n > m then n else m

max3 :: Int -> Int -> Int -> Int
max3 x y z = Main.max z (Main.max x y)

min :: Int -> Int -> Int
min x y = if Main.max x y == x then y else x
