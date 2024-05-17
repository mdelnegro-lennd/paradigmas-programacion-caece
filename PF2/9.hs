data ArbBin rotulo = ArbBinVacio | Nodo rotulo (ArbBin rotulo) (ArbBin rotulo)

instance Show rotulo => Show (ArbBin rotulo) where
    show ArbBinVacio = "{}"
    show (Nodo r left right) = "Nodo " ++ show r ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

nroNodos :: ArbBin rotulo -> Int
nroNodos ArbBinVacio = 0
nroNodos (Nodo rotulo left right) = 1 + nroNodos left + nroNodos right

nroHojas :: ArbBin rotulo -> Int
nroHojas ArbBinVacio = 0
nroHojas (Nodo rotulo ArbBinVacio ArbBinVacio) = 1
nroHojas (Nodo rotulo left right) = nroHojas left + nroHojas right

altura :: ArbBin rotulo -> Int
altura ArbBinVacio = 0
altura (Nodo rotulo ArbBinVacio ArbBinVacio) = 1
altura (Nodo rotulo left right) = max (1 + altura left) (1 + altura right)

preOrder :: ArbBin rotulo -> [rotulo]
preOrder ArbBinVacio = []
preOrder (Nodo rotulo left right) = rotulo : (preOrder left ++ preOrder right)

inOrder :: ArbBin rotulo -> [rotulo]
inOrder ArbBinVacio = []
inOrder (Nodo rotulo left right) =  inOrder left ++ [rotulo] ++ inOrder right

postOrder :: ArbBin rotulo -> [rotulo]
postOrder ArbBinVacio = []
postOrder (Nodo rotulo left right) =  postOrder left ++ postOrder right ++ [rotulo]

igEstrucArb :: ArbBin rotulo -> ArbBin rotulo -> Bool
igEstrucArb ArbBinVacio ArbBinVacio = True
igEstrucArb ArbBinVacio _ = False
igEstrucArb _ ArbBinVacio = False
igEstrucArb (Nodo rotulo1 left1 right1) (Nodo rotulo2 left2 right2) = igEstrucArb left1 left2 && igEstrucArb right1 right2