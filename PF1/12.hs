
perfectoSuma :: Int -> Int -> Int
perfectoSuma n 0 = 0
perfectoSuma n m 
    | n `mod` m == 0 = m + perfectoSuma n (m-1) 
    | otherwise = perfectoSuma n (m-1)

perfecto :: Int -> Bool
perfecto 1 = True
perfecto n = perfectoSuma n n - n == n
