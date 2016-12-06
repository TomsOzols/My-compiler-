module AMTYPES where 

data Command =  PUSH Float | ADD | MULT | SUB | DIV 
                | TRUE | FALSE | EQ | LE | AND | OR | NEG
                | FETCH String | STORE String | NOOP
                | BRANCH [Command] [Command]
                | LOOP [Command] [Command]
                