module Compiler where

import AMTYPES

-- Arithmetic types and expressions
data Aexp = Acon Float | Var String | Plus Aexp Aexp |
            Minus Aexp Aexp | Times Aexp Aexp | Divide Aexp Aexp

aval :: Aexp -> [AMTYPES.Command] -> [AMTYPES.Command]
bval :: Bexp -> [AMTYPES.Command] -> [AMTYPES.Command]
eval :: Com -> [AMTYPES.Command] -> [AMTYPES.Command]

aval (Acon a)     codeList = codeList ++ [AMTYPES.PUSH a]
aval (Var a)      codeList = codeList ++ [AMTYPES.FETCH a]
aval (Plus a b)   codeList = codeList ++ aval b codeList ++ aval a codeList ++ [AMTYPES.ADD]
aval (Minus a b)  codeList = codeList ++ aval b codeList ++ aval a codeList ++ [AMTYPES.SUB]
aval (Times a b)  codeList = codeList ++ aval b codeList ++ aval a codeList ++ [AMTYPES.MULT]
aval (Divide a b) codeList = codeList ++ aval b codeList ++ aval a codeList ++ [AMTYPES.DIV]

-- Binary types and expressions
data Bexp = Bcon Bool | Eq Aexp Aexp | Le Aexp Aexp | Ge Aexp Aexp |
            Not Bexp | And Bexp Bexp | Or Bexp Bexp | Neq Aexp Aexp | Less Aexp Aexp | More Aexp Aexp

-- Could make the Not more generic.
bval (Bcon x)    codeList = let value = if x == True then AMTYPES.TRUE else AMTYPES.FALSE in codeList ++ [value]
bval (Eq a b)    codeList = codeList ++ aval b codeList ++ aval a codeList ++ [AMTYPES.EQ]
bval (Neq a b)   codeList = (bval (Eq a b) codeList) ++ [AMTYPES.NEG]
bval (Le a b)    codeList = codeList ++ aval b codeList ++ aval a codeList ++ [AMTYPES.LE]
bval (Ge a b)    codeList = (bval (Le a b) codeList) ++ [AMTYPES.NEG] ++ (bval (Eq a b) codeList) ++ [AMTYPES.OR]
bval (Less a b)  codeList = (bval (Eq a b) codeList) ++ [AMTYPES.NEG] ++ (bval (Le a b) codeList) ++ [AMTYPES.OR]
bval (More a b)  codeList = (bval (Le a b) codeList) ++ [AMTYPES.NEG]
bval (Not b)     codeList = codeList ++ bval b codeList ++ [AMTYPES.NEG]
bval (And b1 b2) codeList = codeList ++ bval b1 codeList ++ bval b2 codeList ++ [AMTYPES.AND]
bval (Or b1 b2)  codeList = codeList ++ bval b1 codeList ++ bval b2 codeList ++ [AMTYPES.OR]

-- Program types and expressions
data Com = Skip | Assign String Aexp | Seq Com Com |
           If Bexp Com Com | While Bexp Com 

eval (Skip)        codeList = codeList ++ [AMTYPES.NOOP]
eval (Assign x a)  codeList = codeList ++ aval a codeList ++ [AMTYPES.STORE x]
eval (Seq c1 c2)   codeList = codeList ++ eval c1 codeList ++ eval c2 codeList
eval (If b c1 c2)  codeList = codeList ++ bval b codeList ++ [AMTYPES.BRANCH (eval c1 []) (eval c2 [])]
eval (While b c)   codeList = codeList ++ [AMTYPES.LOOP (bval b []) (eval c [])]


runProgram program = eval program []

-- Programs
gcd_program = Seq cSeqSeq cSeq3 where   ax = Var "x"
                                        ay = Var "y"
                                        b1 = Not (Eq ax ay)
                                        b2 = Ge ax ay
                                        cSeqSeq = Seq cSeq1 cSeq2
                                        cSeq1 = Assign "x" (Acon 15)
                                        cSeq2 = Assign "y" (Acon 5)
                                        cSeq3 = While b1 c3
                                        c1 = Assign "x" (Minus ax ay)
                                        c2 = Assign "y" (Minus ay ax)
                                        c3 = If b2 c1 c2

otherProgram = Seq c1 c2 where ax = Var "x"
                               ay = Var "y"
                               c1 = Assign "x" (Acon 7)
                               c2 = Seq c3 c6
                               c3 = Seq c4 c5
                               c4 = Assign "y" (Acon 23)
                               c5 = Assign "y" (Minus ay ax)
                               c6 = Skip

anotherProgram = Seq c1 c2 where ax = Var "x"
                                 ay = Var "y"
                                 c1 = While b1 c3
                                 c2 = Assign "y" (Minus ay ax)
                                 b1 = (Bcon True)
                                 c3 = Assign "x" (Acon 14.6)

justLoop = While b1 c1 where ax = Var "x"
                             b1 = (Bcon False)
                             c1 = Assign "x" (Acon 1006.4)

stupidProgram = Seq c1 c2 where ax = Var "x"
                                c1 = Assign "x" (Acon 515)
                                c2 = Assign "x" (Plus ax (Acon 100))

aDifferentStupidProgram = Seq c1 c2 where ax = Var "x"
                                          c1 = Assign "x" (Acon 11)
                                          c2 = Skip

aProgramWithIfs = Seq c1 c2 where ax = Var "x"
                                  ay = Var "y"
                                  aC1_1 = Var "c1_1"
                                  aC1_2 = Var "c1_2"
                                  aC2_1 = Var "c2_1"
                                  aC2_2 = Var "c2_2"
                                  c1 = Assign "x" (Acon 5)
                                  c2 = Seq c3 cSeq
                                  c3 = Seq c4 c5
                                  c4 = Assign "y" (Acon 7)
                                  b1 = (Bcon False)
                                  b2 = (Bcon True)
                                  c5 = If b1 c1_1 c1_2
                                  cSeq = Seq c6 cNoop
                                  cNoop = Skip
                                  c6 = If b2 c2_1 c2_2
                                  c1_1 = Assign "c1_1" (Acon 3000)
                                  c1_2 = Assign "c1_2" (Acon (-3000))
                                  c2_1 = Assign "c2_1" (Acon 6700)
                                  c2_2 = Assign "c2_2" (Acon (-6700))

nestedIfs = Seq c1 c2 where ax = Var "x"
                            c1 = Assign "x" (Acon 1)
                            c2 = If b1 c1_1 c1_2
                            b1 = (Bcon False)
                            c1_1 = If b2 c2_1 c2_2
                            c1_2 = Assign "x" (Acon 1000)
                            b2 = (Bcon True)
                            c2_1 = Assign "x" (Acon (-1))
                            c2_2 = Assign "x" (Acon 2)

nestedIfsInv = Seq c1 c2 where      ax = Var "x"
                                    aC1_1 = Var "c1_1"
                                    aC2_1 = Var "c2_1"
                                    aC2_2 = Var "c2_2"
                                    c1 = Assign "x" (Acon 1)
                                    c2 = If b1 c1_1 c1_2
                                    b1 = (Bcon False)
                                    c1_1 = Assign "c1_1" (Acon 1000)
                                    c1_2 = If b2 c2_1 c2_2
                                    b2 = (Bcon True)
                                    c2_1 = Assign "c2_1" (Acon (-1))
                                    c2_2 = Assign "c2_2" (Acon 2)

aWhileProg = Seq c1 cSeq where  ax = Var "x"
                                ay = Var "y"
                                c1 = Assign "x" (Acon 1)
                                c2 = Assign "y" (Acon 1)
                                cSeq = Seq c2 c3
                                c3 = While b1 cWhile
                                b1 = Eq ax ay
                                cWhile = Assign "x" (Plus ax (Acon 1))

aWhileFalse = Seq c1 cSeq where ax = Var "x"
                                ay = Var "y"
                                c1 = Assign "x" (Acon 1)
                                c2 = Assign "y" (Acon 16)
                                cSeq = Seq c2 c3
                                c3 = While b1 cWhile
                                b1 = (Bcon False)
                                cWhile = Assign "x" (Plus ax (Acon 1))

complexBooleans = Seq c1 c2 where ax = Var "x"
                                  c1 = Assign "x" (Acon 1)
                                  c2 = While b1 cIf
                                  b1 = And (Bcon True) (Le ax (Acon 9))
                                  cIf = If b2 c1_1 c1_2
                                  c1_1 = Assign "x" (Plus ax (Acon 3))
                                  c1_2 = Assign "x" (Plus ax (Acon 2))
                                  b2 = Or (Eq (Divide ax (Acon 3)) (Acon 1)) (Eq (Times ax (Acon 2)) (Acon 12))