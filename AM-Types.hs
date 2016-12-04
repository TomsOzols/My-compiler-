module AMTYPES where 

data Command =  PUSH Float | ADD | MULT | SUB | DIV 
                | TRUE | FALSE | EQ | LE | GE | AND | OR | NEG | NEQ | LESSER | GREATER
                | FETCH String | STORE String | NOOP 
                | BRANCH [Command] [Command]
                | LOOP [Command] [Command]
                | END -- A dirty hack for the interpreter