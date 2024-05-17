data Extension a = Vacio | Solo a deriving (Show)

miExtension :: Extension Int -> Int
miExtension Vacio = 2
miExtension (Solo 0) = 1