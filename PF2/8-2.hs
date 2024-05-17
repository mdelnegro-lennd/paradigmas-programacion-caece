data Cola a = ColaVacia | Nodo a (Cola a) deriving Show

esVacia :: Cola a -> Bool
esVacia ColaVacia = True
esVacia _ = False

cabeza :: Cola a -> a
cabeza (Nodo num ColaVacia) = num
cabeza (Nodo num cola) = cabeza cola

agregar :: Cola a -> a -> Cola a
agregar ColaVacia n = Nodo n ColaVacia
agregar cola@(Nodo num _) n = Nodo n cola

sacar :: Cola a -> Cola a
sacar (Nodo num (Nodo num2 ColaVacia)) = Nodo num ColaVacia
sacar (Nodo num cola) = Nodo num (sacar cola)

cantidad :: Cola a -> Int
cantidad ColaVacia = 0
cantidad (Nodo a cola) = 1 + cantidad cola
