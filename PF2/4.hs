data Nat = Cero | Suc Nat deriving (Show)

instance Eq Nat where
    Cero == Cero = True
    Cero == _ = False
    _ == Cero = False

instance Ord Nat where
    Cero <= _          = True
    _ <= Cero          = False
    (Suc n) <= (Suc m) = n <= m


suma :: Nat -> Nat -> Nat
suma Cero Cero = Cero
suma Cero m = m
suma n Cero = n
suma (Suc n) m = Suc (suma n m)

resta :: Nat -> Nat -> Nat
resta n Cero = n
resta (Suc n) (Suc m) = resta n m

producto :: Nat -> Nat -> Nat
producto Cero _ = Cero
producto _ Cero = Cero
producto (Suc Cero) m = m
producto n (Suc Cero) = n
producto (Suc n) m = producto n (suma m m)

convertirNatInt :: Nat -> Int
convertirNatInt Cero = 0
convertirNatInt (Suc n) = 1 + convertirNatInt n

convertirIntNat :: Int -> Nat
convertirIntNat 0 = Cero
convertirIntNat n = Suc (convertirIntNat (n - 1))
