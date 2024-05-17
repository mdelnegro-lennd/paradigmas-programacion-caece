data Conjunto a = ConjuntoVacio | Conjunto [a] deriving Show

union :: Conjunto a -> Conjunto a -> Conjunto a
union (Conjunto lista1) (Conjunto lista2) = Conjunto (lista1 ++ lista2)

esVacio :: Conjunto a -> Bool
esVacio ConjuntoVacio = True
esVacio (Conjunto []) = True
esVacio (Conjunto _) = False

agElem :: a -> Conjunto a -> Conjunto a
agElem elemento (Conjunto lista) = Conjunto (elemento : lista)

quitaElem :: Eq a => a -> Conjunto a -> Conjunto a
quitaElem elemento ConjuntoVacio = Conjunto []
quitaElem elemento (Conjunto []) = Conjunto []
quitaElem elemento (Conjunto lista@(x:xs)) = 
    if elemento == x 
    then quitaElem elemento (Conjunto xs)
    else agElem x (quitaElem elemento (Conjunto xs))

pertenece :: Eq a => a -> Conjunto a -> Bool
pertenece el ConjuntoVacio = False
pertenece el (Conjunto []) = False
pertenece el (Conjunto (x:xs)) = if el == x then True else pertenece el (Conjunto xs)