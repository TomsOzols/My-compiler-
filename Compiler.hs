module Imp where

-- Arithmetic types and expressions
data Aexp = Acon Integer | Var String | Plus Aexp Aexp |
            Minus Aexp Aexp | Times Aexp Aexp 

aval (Acon a) state = a
aval (Var a) state = state a
aval (Plus a b) state = (aval a state) + (aval b state)
aval (Minus a b) state = (aval a state) - (aval b state)
aval (Times a b) state = (aval a state) * (aval b state)
aval (Divide a b) state = (aval )

-- Binary types and expressions
data Bexp = Bcon Bool | Eq Aexp Aexp | Ge Aexp Aexp |
            Not Bexp | And Bexp Bexp | Or Bexp Bexp 

bval (Bcon x) state = x
bval (Eq a b) state = c where c = ((aval a state) == (aval b state))
bval (Ge a b) state = c where c = ((aval a state) >= (aval b state))
bval (Not b) state = not (bval b state)
bval (And b1 b2) state = bval b1 state && bval b2 state
bval (Or b1 b2) state = bval b1 state || bval b2 state

-- Program types and expressions
data Com = Skip | Assign String Aexp | Seq Com Com |
           If Bexp Com Com | While Bexp Com 

eval (Skip) state = state -- (1)
eval (Assign x a) state = let v = (aval a state) in \y -> if x==y then v else (state y) -- (2)
eval (Seq c1 c2) state  = eval c2 (eval c1 state) -- (3)
eval (If b c1 c2) state = if (bval b state) then (eval c1 state) -- (4a)
                                      else (eval c2 state) -- (4b)
eval (While b c) state  = if (bval b state) then eval (While b c) (eval c state) -- (5b)
                                     else state -- (5a)

-- State store
mk_state ((v,val):t) x = if v == x then val
                                   else (mk_state t x)
mk_state null x = 0

show_value state x = (x,state(x))
show_state state l = map (show_value state) l

-- Entry point
compile c state = show_state (eval c (mk_state state)) (map fst state)