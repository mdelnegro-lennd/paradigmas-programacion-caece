data LambdaTerm = 
    Var Char |
    Apl LambdaTerm LambdaTerm | 
    Abs Char LambdaTerm deriving (Show)

subExp :: LambdaTerm -> [LambdaTerm]
subExp (Var x) = [Var x]
subExp (Apl exp1 exp2) = subExp exp1 ++ subExp exp2
subExp (Abs x exp) = exp : subExp exp

varLibs :: LambdaTerm -> [Char]
varLibs (Var x) = [x]
varLibs (Apl exp1 exp2) = varLibs exp1 ++ varLibs exp2
varLibs (Abs absVar exp) = eliminar absVar (varLibs exp)

sust :: LambdaTerm -> Char -> LambdaTerm -> LambdaTerm
sust (Var x) desde hacia
    | x == desde = hacia
    | otherwise = Var x
sust (Apl exp1 exp2) desde hacia = Apl (sust exp1 desde hacia) (sust exp2 desde hacia)
sust (Abs absVar exp) desde hacia
    | absVar == desde = Abs absVar exp
    | otherwise = Abs absVar (sust exp desde hacia)

betaRed :: LambdaTerm -> LambdaTerm
betaRed (Apl (Abs x exp1) exp2) = sust exp1 x exp2
betaRed term = term

eliminar :: Char -> [Char] -> [Char]
eliminar _ [] = []
eliminar var (x:xs)
    | var == x = eliminar var xs
    | otherwise = x : eliminar var xs
