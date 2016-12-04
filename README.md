# MyCompiler-

A haskell based compiler of the abstract language IMP_D to the Abstract machine AM
And an interpreter for the Abstract machine AM

My first real attempt at currying haskell

To load the thing in Prelude:
:l Entry2.hs Compiler.hs "AM-Types.hs" "AM-Interpreter.hs"

After it is loaded you can run any of the predefined programs:
run3
runIfs
runNestedIfs
runGcd
runWhileProg

Or god forbids You can define Your own program.

Entry1.hs is an incomplete AM command stringifier. Was used in the development/debugging of the compiler/interpreter.