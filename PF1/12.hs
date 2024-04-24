
perfectoSuma :: Int -> Int -> Int
perfectoSuma n 0 = 0
perfectoSuma n m = if n `mod` m == 0 then m + perfectoSuma n (m-1) else perfectoSuma n (m-1)

perfecto :: Int -> Bool
perfecto 1 = True
perfecto n = perfectoSuma n n - n == n
