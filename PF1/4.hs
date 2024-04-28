esVacia :: [a] -> Bool
esVacia [] = True
esVacia lista = False

cabeza :: [a] -> a
cabeza (x:_) = x

cola :: [a] -> [a]
cola (_:ys) = ys