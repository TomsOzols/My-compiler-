module BS where 

import AMTYPES

code :: [AMTYPES.Command]
code = []

putStrings = AMTYPES.ADD : code
putStrings2 = AMTYPES.SUB : code

run = let v = putStrings!!0 in 
        let x = putStrings2!!0 in
    AMTYPES.value v ++ AMTYPES.value x