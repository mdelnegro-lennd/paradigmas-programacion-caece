signo :: Int -> Int
signo n 
    | n > 0 = 1 
    | n == 0 = 0 
    | otherwise = -1

negativo :: Int -> Bool
negativo n 
    | signo(n) == -1 = True 
    | otherwise = False