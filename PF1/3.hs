factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

combinatorio :: Int -> Int -> Int
combinatorio n k = factorial n `div` (factorial (n-k) * factorial k)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = n + fibonacci (n-1)

divisiblePor :: Int -> Int -> Bool
divisiblePor x y = mod x y == 0
