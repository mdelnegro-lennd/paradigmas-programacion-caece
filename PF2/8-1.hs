data Pila a = PilaVacia | Nodo a (Pila a) deriving Show

esVacia :: Pila a -> Bool
esVacia PilaVacia = True
esVacia _ = False

cabeza :: Pila a -> a
cabeza (Nodo num pila) = num

agregar :: Pila a -> a -> Pila a
agregar PilaVacia n = Nodo n PilaVacia
agregar pila@(Nodo num _) n = Nodo n pila

sacar :: Pila a -> Pila a
sacar PilaVacia = PilaVacia
sacar (Nodo num pila) = pila

cantidad :: Pila a -> Int
cantidad PilaVacia = 0
cantidad (Nodo a pila) = 1 + cantidad pila
