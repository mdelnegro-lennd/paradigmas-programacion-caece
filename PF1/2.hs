max :: Int -> Int -> Int
max n m = if n > m then n else m

max3 :: Int -> Int -> Int -> Int
max3 x y z = Main.max z (Main.max x y)

min :: Int -> Int -> Int
min x y = if Main.max x y == x then y else x
