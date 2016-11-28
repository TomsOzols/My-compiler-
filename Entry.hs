module Entry where

import Compiler
import AMTYPES

doTheBoogy = map stringIt Compiler.gcd_program_wrap

stringIt (AMTYPES.PUSH x) = "PUSH"
stringIt (AMTYPES.ADD) = "ADD"
stringIt (AMTYPES.MULT) = "MULT"
stringIt (AMTYPES.SUB) = "SUB"
stringIt (AMTYPES.DIV) = "DIV"
stringIt (AMTYPES.TRUE) = "TRUE"
stringIt (AMTYPES.FALSE) = "FALSE"
stringIt (AMTYPES.EQ) = "EQ"
stringIt (AMTYPES.LE) = "LE"
stringIt (AMTYPES.GE) = "GE"
stringIt (AMTYPES.AND) = "AND"
stringIt (AMTYPES.OR) = "OR"
stringIt (AMTYPES.NEG) = "NEG"
stringIt (AMTYPES.NEQ) = "NEQ"
stringIt (AMTYPES.LESSER) = "LESSER"
stringIt (AMTYPES.GREATER) = "GREATER"
stringIt (AMTYPES.FETCH x) = "FETCH"
stringIt (AMTYPES.STORE x) = "STORE"
stringIt (AMTYPES.NOOP) = "NOOP"
stringIt (AMTYPES.BRANCH c1 c2) = "BRANCH"
stringIt (AMTYPES.LOOP b c) = "LOOP"