instance Show rotulo => Show (ArbBinRotHoj rotulo) where
    show ArbBinRotHojVacio = "{}"
    show (Hoja rotulo) = "Hoja " ++ show rotulo
    show (Nodo left right) = "Nodo " ++ " (" ++ show left ++ ") (" ++ show right ++ ")"


data ArbBinRotHoj rotulo = ArbBinRotHojVacio | Hoja rotulo | Nodo (ArbBinRotHoj rotulo) (ArbBinRotHoj rotulo)