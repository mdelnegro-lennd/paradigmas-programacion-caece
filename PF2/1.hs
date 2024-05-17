-- hanoi :: Int -> Int
-- hanoi 1 = 1
-- hanoi discos = 2 ^ (discos - 1) + hanoi (discos - 1)

hanoi :: Int -> Poste -> Poste -> Poste -> [Movimiento]
hanoi 0 _ _ _ = []
hanoi 1 origen _ destino = [Movimiento origen destino]
hanoi discos origen aux destino = 
    let 
        todosAlAux = hanoi (discos - 1) origen destino aux
        movimientoActual = [Movimiento origen destino]
        todosAlDestino = hanoi (discos - 1) aux origen destino
    in todosAlAux ++ movimientoActual ++ todosAlDestino

data Poste = A | B | C deriving (Show)

data Movimiento = Movimiento Poste Poste deriving (Show)
