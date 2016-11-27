module AM_Interpreter where

data Command = PUSH Integer | ADD | MULT | SUB | DIV | TRUE | FALSE | EQ | LE | AND | OR | NEG | FETCH String | STORE String | NOOP | BRANCH Command Command | LOOP Command Command

exp (PUSH number) code stack state = number : stack
-- exp Add code stack state = let first = head:tail in let second =



getCode (code, _, _) = code
getStack (_, stack, _) = stack
getState (_, _, state) = state


mk_globals ((v,val):t) = \x -> if v == x then val
                                   else (mk_globals t x)
mk_globals null = \x -> 0

show_value s = \x -> (x,s(x))
show_globals s = \l -> map (show_value s) l

run c s = show_globals (exp c (mk_globals s)) (map fst s)
