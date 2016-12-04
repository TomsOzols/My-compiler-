module Entry where

import Compiler
import AMTYPES

run3 = map stringIt (Compiler.runProgram (Compiler.otherProgram))
runIfs = map stringIt (Compiler.runProgram (Compiler.aProgramWithIfs))
runNestedIfs = map stringIt (Compiler.runProgram (Compiler.nestedIfs))
runNestedIfsInv = map stringIt (Compiler.runProgram (Compiler.nestedIfsInv))
runGcd = map stringIt (Compiler.runProgram (Compiler.gcd_program))
runWhileProg = map stringIt (Compiler.runProgram (Compiler.aWhileProg))

stringIt :: Command -> [String]
stringIt (AMTYPES.PUSH x) = ["PUSH", show x]
stringIt (AMTYPES.ADD) = ["ADD", ""]
stringIt (AMTYPES.MULT) = ["MULT", ""]
stringIt (AMTYPES.SUB) = ["SUB", ""]
stringIt (AMTYPES.DIV) = ["DIV", ""]
stringIt (AMTYPES.TRUE) = ["TRUE", ""]
stringIt (AMTYPES.FALSE) = ["FALSE", ""]
stringIt (AMTYPES.EQ) = ["EQ", ""]
stringIt (AMTYPES.LE) = ["LE", ""]
stringIt (AMTYPES.GE) = ["GE", ""]
stringIt (AMTYPES.AND) = ["AND", ""]
stringIt (AMTYPES.OR) = ["OR", ""]
stringIt (AMTYPES.NEG) = ["NEG", ""]
stringIt (AMTYPES.NEQ) = ["NEQ", ""]
stringIt (AMTYPES.LESSER) = ["LESSER", ""]
stringIt (AMTYPES.GREATER) = ["GREATER", ""]
stringIt (AMTYPES.FETCH x) = ["FETCH", show x]
stringIt (AMTYPES.STORE x) = ["STORE", show x]
stringIt (AMTYPES.NOOP) = ["NOOP", ""]
stringIt (AMTYPES.BRANCH c1 c2) = ["BRANCH", unwords (stringerify c1 ++ stringerify c2)]
stringIt (AMTYPES.LOOP b c) = ["LOOP", unwords (stringerify b ++ stringerify c)]

stringerify :: [Command] -> [String]
stringerify commands = map unwords (map stringIt commands)

group :: [(Integer, [Char])] -> [[Char]]
group ((n, str):ls) = let
      (children, rest) = span (\(m, _) -> m > n) ls
      subgroups = map (str ++) $ group children
   in if null children then [str] ++ group rest
      else subgroups ++ group rest
group [] = []