module AM_Interpreter where

import AMTYPES

data StackValue = AFloatValue Float | ABoolValue Bool

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Hell
express :: AMTYPES.Command -> ([AMTYPES.Command], [StackValue], [(String, Float)]) -> ([AMTYPES.Command], [StackValue], [(String, Float)])
express (AMTYPES.PUSH x) = \configuration -> pushToStack configuration (AFloatValue x)
express (AMTYPES.ADD) = \configuration -> pop2push1 (floatFunction (+)) configuration AFloatValue
express (AMTYPES.MULT) = \configuration -> pop2push1 (floatFunction (*)) configuration AFloatValue
express (AMTYPES.SUB) = \configuration -> pop2push1 (floatFunction (-)) configuration AFloatValue
express (AMTYPES.DIV) = \configuration -> pop2push1 (floatFunction (/)) configuration AFloatValue
express (AMTYPES.TRUE) = \configuration -> pushToStack configuration (ABoolValue True)
express (AMTYPES.FALSE) = \configuration -> pushToStack configuration (ABoolValue False)
express (AMTYPES.EQ) = \configuration -> pop2push1 (floatFunction (==)) configuration ABoolValue
express (AMTYPES.LE) = \configuration -> pop2push1 (floatFunction (>=)) configuration ABoolValue
-- express (AMTYPES.GE)
express (AMTYPES.AND) = \configuration -> pop2push1 (booleanFunction (&&)) configuration ABoolValue
express (AMTYPES.OR) = \configuration -> pop2push1 (booleanFunction (||)) configuration ABoolValue
express (AMTYPES.NEG) = \configuration ->
    let currentConfig = popFromStack configuration in
        let truthValue = convertStackValueToBool (fst currentConfig) in
            let newTruthValue = if truthValue == True then False else True in
                pushToStack (snd currentConfig) (ABoolValue newTruthValue)
-- express (AMTYPES.NEQ)
-- express (AMTYPES.LESSER)
-- express (AMTYPES.GREATER)
express (AMTYPES.FETCH x) = \configuration -> pushToStack (configuration) (AFloatValue (fst (getStateVariable configuration x)))
express (AMTYPES.STORE x) = \configuration ->
    let newState = popFromStack configuration in
        applyToState (createVarOrChangeValue x (convertStackValueToFloat (fst (newState)))) (snd newState)
express (AMTYPES.NOOP) = \configuration -> configuration
express (AMTYPES.BRANCH c1 c2) = \configuration -> 
    let newState = popFromStack configuration in
        if convertStackValueToBool (fst newState)
            then addToCode (snd newState) c1
            else addToCode (snd newState) c2
express (AMTYPES.LOOP b c) = \configuration -> addToCode configuration (b ++ [(AMTYPES.BRANCH (c ++ [(AMTYPES.LOOP b c)]) [AMTYPES.NOOP])])
express null = \configuration -> configuration
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
addToCode :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> [AMTYPES.Command] -> ([AMTYPES.Command], [StackValue], [(String, Float)])
addToCode configuration newCode = (newCode ++ (getCode configuration), getStack configuration, getState configuration)
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Functions that apply a function to 2 float/boolean values
floatFunction :: (Float -> Float -> t) -> StackValue -> StackValue -> t
floatFunction function a1 a2 = function (convertStackValueToFloat a1) (convertStackValueToFloat a2)
booleanFunction :: (Bool -> Bool -> Bool) -> StackValue -> StackValue -> Bool
booleanFunction function b1 b2 = function (convertStackValueToBool b1) (convertStackValueToBool b2)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Stupid casting functions
convertStackValueToFloat (AFloatValue f) = f
convertStackValueToBool (ABoolValue b) = b
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- configuration is a structure that stores the 3 configuration lists (code, stack, state)
-- the types of the configuration triple: ([AMTYPES.Command], [StackValue], [(String, Float)])
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Stack related functions
-- Paramater asType is the corresponding StackValue constructor
pop2push1 :: (StackValue -> StackValue -> t) -> ([Command], [StackValue], [(String, Float)]) -> (t -> StackValue) -> ([Command], [StackValue], [(String, Float)])
pop2push1 function configuration asType =
    let firstOp = popFromStack configuration in
        let secondOp = popFromStack (snd firstOp) in
            pushToStack (snd secondOp) (asType (function (fst firstOp) (fst secondOp)))

pushToStack :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> StackValue -> ([AMTYPES.Command], [StackValue], [(String, Float)])
pushToStack configuration value = (getCode configuration, [value] ++ (getStack configuration), getState configuration)

popFromStack :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> (StackValue, ([AMTYPES.Command], [StackValue], [(String, Float)]))
popFromStack (code, (first:stackTail), state) = (first, (code, stackTail, state))
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Functions to change the value of a variable or create a new variable/value if one doesn't already exist
createVarOrChangeValue :: String -> Float -> ([(String, Float)] -> [(String, Float)])
createVarOrChangeValue varName number = \currentState -> if doesListContainVariable currentState varName
    then map (applyIfVarName varName number) currentState
    else [(varName, number)] ++ currentState

applyIfVarName :: Eq t => t -> t1 -> (t, t1) -> (t, t1)
applyIfVarName varName number = \current -> if fst current == varName
    then (fst current, number)
    else current

-- Probably unneeded. Could be a let currently.
applyToState :: ([(String, Float)] -> [(String, Float)]) -> ([AMTYPES.Command], [StackValue], [(String, Float)]) -> ([AMTYPES.Command], [StackValue], [(String, Float)])
applyToState function configuration = (getCode configuration, getStack configuration, function (getState configuration))
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Allows us to get the value of a variable inside of state
getStateVariable :: ([Command], [StackValue], [(String, Float)]) -> String -> (Float, ([Command], [StackValue], [(String, Float)]))
getStateVariable configuration varName =
    let state = getState configuration in
        ((findVariableByName state varName), configuration)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Key/Value list walking functions
doesListContainVariable ((key, value):xs) varName = if key == varName then True else doesListContainVariable xs varName
doesListContainVariable null varName = False

findVariableByName :: Eq t => [(t, Float)] -> t -> Float
findVariableByName ((key, value):xs) varName = if key == varName then value else findVariableByName xs varName
findVariableByName null varName = error "No such variable"
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Functions to access the different parts of the configuration
getCode :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> [AMTYPES.Command]
getCode (code, _, _) = code

getStack :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> [StackValue]
getStack (_, stack, _) = stack

getState :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> [(String, Float)]
getState (_, _, state) = state
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- The code walker
moveThroughCode :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> (AMTYPES.Command, ([AMTYPES.Command], [StackValue], [(String, Float)]))
moveThroughCode (x:xs, stack, state) = (x, (xs, stack, state))

walkCode :: ([AMTYPES.Command], [StackValue], [(String, Float)]) -> ([AMTYPES.Command], [StackValue], [(String, Float)])
walkCode configuration = let codePieceWithconfiguration = moveThroughCode configuration in
    let walkResult = walkCodeBody codePieceWithconfiguration in
        if null (getCode walkResult)
            then walkResult
            else walkCode walkResult

walkCodeBody codePieceWithconfiguration = express (fst codePieceWithconfiguration) (snd codePieceWithconfiguration)
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Entry point for the interpreter
-- runInterpreter :: [Command] -> [(String, Float)]
runInterpreter code = let a = (code, [], []) in getStackAndState (walkCode a)

getStackAndState (_, stack, state) = (map stringify stack, state)

stringify (ABoolValue a) = show a
stringify (AFloatValue a) = show a
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++