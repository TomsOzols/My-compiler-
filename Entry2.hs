module Entry where

import Compiler
import AM_Interpreter

run1 = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.stupidProgram))
run2 = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.aDifferentStupidProgram))
run3 = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.otherProgram))
runIfs = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.aProgramWithIfs))
runNestedIfs = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.nestedIfs))
runNestedIfsInv = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.nestedIfsInv))
runGcd = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.gcd_program))
runWhileProg = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.aWhileProg))
runWhileFalse = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.aWhileFalse))
runComplexBools = AM_Interpreter.runInterpreter (Compiler.runProgram (Compiler.complexBooleans))