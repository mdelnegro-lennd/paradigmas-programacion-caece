--i)	Definir la función esVacia que devuelve si una lista está vacía o no. En Haskell esta función se llama null.
--ii)	Definir las funciones cabeza y resto, que devuelven el primer elemento y del segundo al último elemento de una lista, 
--respectivamente. ¿Pueden realizarse estas funciones sin usar apareamiento de patrones (pattern matching)? 
--En Haskell estas funciones se llaman head y tail respectivamente.

esVacia :: [a] -> Bool
esVacia lista = null lista

cabeza :: [a] -> a
cabeza lista = head lista

cola :: [a] -> [a]
cola lista = tail lista