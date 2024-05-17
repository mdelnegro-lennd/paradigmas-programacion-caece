data TipoDeSangre = APos | ANeg | BPos | BNeg | ABPos | ABNeg | OPos | ONeg deriving (Show)

puedeDonar :: Maybe TipoDeSangre -> Maybe [TipoDeSangre]
puedeDonar Nothing = Nothing
puedeDonar (Just APos) = Just [APos, ANeg, ABPos, ABNeg]
puedeDonar (Just ANeg) = Just [ANeg, ABNeg]
puedeDonar (Just BPos) = Just [BPos, BNeg, ABPos, ABNeg]
puedeDonar (Just BNeg) = Just [BNeg, ABNeg]
puedeDonar (Just OPos) = Just [APos, ANeg, BPos, BNeg, OPos, ONeg]
puedeDonar (Just ONeg) = Just [ONeg, ANeg, BNeg]
