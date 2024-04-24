esVacia :: [a] -> Bool
esVacia [] = True
esVacia lista = False

cabeza :: [a] -> a
cabeza (x:ys) = x

cola :: [a] -> [a]
cola (x:ys) = ys