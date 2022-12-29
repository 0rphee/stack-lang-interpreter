{-# LANGUAGE TupleSections #-}
module Lib where

import Data.Bifunctor (first)

data Operation = Mult | Add
                 deriving (Eq, Show)

data Instruction = Const Int
                 | Op Operation
                 deriving (Eq, Show)

newtype Error = OperationError String
                 deriving (Eq, Show)

newtype Machine = Machine [Instruction]
                deriving (Eq, Show)

-- | Adds an 'Instruction' to the stack of the 'Machine'.
push :: Machine  -> Instruction -> Machine
push (Machine xs) instruction = Machine $ instruction:xs

-- | Returns the first element of the 'Machine' stack, and removes the element from it.
pop :: Machine  -> (Machine , Maybe Instruction)
pop (Machine []) = (Machine [], Nothing)
pop (Machine (x:xs)) = (Machine xs, Just x)

-- | Higher-order function for operations with Ints in constants
numOperation :: String -> (Int -> Int -> Int ) -> Instruction -> Instruction -> Either Error Instruction
numOperation _ f (Const x) (Const y) = Right $ Const (f x y)
-- In practice, this errors never show up. But I haven't got around to fix the logic and remove this
numOperation operName _ (Const _) (Op _) = Left $ OperationError $ "Operation " ++ operName ++ "Expected 2 values, but received a value and an operation"
numOperation operName _ (Op _) (Const _) = Left $ OperationError $ "Operation " ++ operName ++ " Expected 2 values, but received a value and an operation"
numOperation operName _ (Op _) (Op _) = Left $ OperationError $ "Operation " ++ operName ++ " Expected 2 values, but received 2 operations"

add :: Instruction -> Instruction -> Either Error Instruction
add = numOperation "'+'" (+)

mult :: Instruction -> Instruction -> Either Error Instruction
mult = numOperation "'*'" (*)

-- | Performs an operation on the stack. 
-- If perfomed successfully, returns the updated 'Machine'. 
-- If an error occurs, Returns the error and the state of the Machine.
operate :: Machine -> Operation -> Either (Error, Machine) Machine
operate machine op = case result of
                          Just a -> first (,m'') a
                          Nothing -> Left ( OperationError
                                            "Not enough values on the stack to perform operation"
                                          , m'' )
  where (m', right) = pop machine
        (m'', left) = pop m'
        result = (fmap . fmap) (push m'') $
          case op of
            Mult -> mult <$> left <*> right
            Add -> add <$> left <*> right

-- | Executes recursively the Instructions given with the 'Machine'
execute' :: Either (Error, Machine) Machine -> [Instruction]
         -> Either (Error, Machine) Machine
execute' (Left errMachine) _ = Left errMachine
execute' machine [] = machine
execute' (Right machine)((Const a):xs) = let m' = push machine $ Const a
                                         in execute' (Right m') xs
execute' (Right machine) ((Op op):xs) = let eith = operate machine op
                                         in execute' eith xs

execute :: [Instruction] -> Either (Error, Machine) Machine
execute = execute' (Right (Machine []))

-- | Pretty-printed result of the instructions
showResult :: Either (Error, Machine) Machine -> String
showResult (Left (err, Machine instructionList)) = "Stack Machine stopped:\t\t\t" ++ show err
                                                  ++ "\nWith final state of Stack Machine:\t"
                                                  ++ show instructionList
showResult (Right (Machine instructionList )) = "Final state of Stack Machine:\t" 
                                             ++ show instructionList
