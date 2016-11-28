module Compiler where

import AMTYPES

-- Arithmetic types and expressions
data Aexp = Acon Float | Var String | Plus Aexp Aexp |
            Minus Aexp Aexp | Times Aexp Aexp | Divide Aexp Aexp

aval :: Aexp -> [AMTYPES.Command] -> [AMTYPES.Command]
bval :: Bexp -> [AMTYPES.Command] -> [AMTYPES.Command]
eval :: Com -> [AMTYPES.Command] -> [AMTYPES.Command]

aval (Acon a)     codeList = codeList ++ [AMTYPES.PUSH a]
aval (Var a)      codeList = codeList ++ [(AMTYPES.FETCH a)]
aval (Plus a b)   codeList = codeList ++ (aval a codeList) ++ (aval b codeList) ++ [AMTYPES.ADD]
aval (Minus a b)  codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.SUB]
aval (Times a b)  codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.MULT]
aval (Divide a b) codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.DIV]

-- Binary types and expressions
data Bexp = Bcon Bool | Eq Aexp Aexp | Le Aexp Aexp | Ge Aexp Aexp |
            Not Bexp | And Bexp Bexp | Or Bexp Bexp | Neq Aexp Aexp | Less Aexp Aexp | More Aexp Aexp

bval (Bcon x)    codeList = let value = if x == True then AMTYPES.TRUE else AMTYPES.FALSE in codeList ++ [value]
bval (Eq a b)    codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.EQ]
bval (Neq a b)   codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.NEQ]
bval (Le a b)    codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.LE]
bval (Ge a b)    codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.GE]
bval (Less a b)  codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.LESSER]
bval (More a b)  codeList = codeList ++ aval a codeList ++ aval b codeList ++ [AMTYPES.GREATER]
bval (Not b)     codeList = codeList ++ bval b codeList ++ [AMTYPES.NEG]
bval (And b1 b2) codeList = codeList ++ bval b1 codeList ++ bval b2 codeList ++ [AMTYPES.AND]
bval (Or b1 b2)  codeList = codeList ++ bval b1 codeList ++ bval b2 codeList ++ [AMTYPES.OR]

-- Program types and expressions
data Com = Skip | Assign String Aexp | Seq Com Com |
           If Bexp Com Com | While Bexp Com 

eval (Skip)        codeList = codeList ++ [AMTYPES.NOOP]
eval (Assign x a)  codeList = codeList ++ aval a codeList ++ [(AMTYPES.STORE x)]
eval (Seq c1 c2)   codeList = codeList ++ eval c1 codeList ++ eval c2 codeList
eval (If b c1 c2)  codeList = codeList ++ bval b codeList ++ [AMTYPES.BRANCH (eval c1 codeList) (eval c2 codeList)]
eval (While b c)   codeList = codeList ++ [AMTYPES.LOOP (bval b codeList) (eval c codeList)]

gcd_program = While b1 c3 where ax = Var "x"
                                ay = Var "y"
                                b1 = Not (Eq ax ay)
                                b2 = Ge ax ay
                                c1 = Assign "x" (Minus ax ay)
                                c2 = Assign "y" (Minus ay ax)
                                c3 = If b2 c1 c2

gcd_program_wrap = eval gcd_program []