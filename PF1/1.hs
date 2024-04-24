signo :: Int -> Int
signo n = if n > 0 then 1 else if n == 0 then 0 else -1

negativo :: Int -> Bool
negativo n = if signo(n) == -1 then True else False