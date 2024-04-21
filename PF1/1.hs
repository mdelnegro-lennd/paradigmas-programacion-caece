--1) Definir la función signo, que dado un entero devuelve 1, 0 ó -1 
--dependiendo si el entero dado es positivo, cero o negativo respectivamente. 
--2) Definir la función negativo, que dado un entero devuelve si es negativo o no. 
--Usar signo.

signo :: Int -> Int
signo n = if n > 0 then 1 else if n == 0 then 0 else -1

negativo :: Int -> Bool
negativo n = if signo(n) == -1 then True else False