instance Show rotulo => Show (ArbGen rotulo) where
    show ArbGenVacio = "{}"
    show (ArbGen rotulo hojas) = "Nodo " ++ show rotulo ++ "(" ++ show hojas ++ ")"

data ArbGen rotulo = ArbGenVacio | ArbGen rotulo [ArbGen rotulo]

nroNodos :: ArbGen rotulo -> Int
nroNodos ArbGenVacio = 0
nroNodos (ArbGen rotulo []) = 1
nroNodos (ArbGen rotulo hijos) = 1 + nroNodosAux hijos

nroNodosAux :: [ArbGen rotulo] -> Int
nroNodosAux [] = 0
nroNodosAux (y:ys) = nroNodos y + nroNodosAux ys

nroHojas :: ArbGen rotulo -> Int
nroHojas ArbGenVacio = 0
nroHojas (ArbGen rotulo []) = 1
nroHojas (ArbGen rotulo hijos) = nroHojasAux hijos

nroHojasAux :: [ArbGen rotulo] -> Int
nroHojasAux [] = 0
nroHojasAux (y:ys) = nroHojas y + nroHojasAux ys

altura :: ArbGen rotulo -> Int
altura ArbGenVacio = 0
altura (ArbGen rotulo []) = 1
altura (ArbGen rotulo hijos) = 1 + alturaAux hijos

alturaAux :: [ArbGen rotulo] -> Int
alturaAux [] = 0
alturaAux (y:ys) = max(altura y) (alturaAux ys)

preOrder :: ArbGen rotulo -> [rotulo]
preOrder ArbGenVacio = []
preOrder (ArbGen rotulo []) = [rotulo]
preOrder (ArbGen rotulo hijos) = rotulo : preOrderAux hijos

preOrderAux :: [ArbGen rotulo] -> [rotulo]
preOrderAux [] = []
preOrderAux (x:xs) = preOrder x ++ preOrderAux xs
