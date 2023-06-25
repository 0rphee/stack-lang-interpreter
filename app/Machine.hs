{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoOverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Machine
  ( runMachine
  , showResult
  ) where
import Control.Monad.Primitive  ( RealWorld )

import Data.Primitive           ( MutableByteArray )
import Data.Primitive.ByteArray

newtype Error
  = OperationError String
  deriving (Eq, Show)

data ByteCodeInstruction
  = Const !Int
  | Op !MachineOperation
  deriving (Eq, Show)

newtype StackValue
  = ConstInt Int
  deriving (Show)

data MachineOperation
  = Mult
  | Add
  deriving (Eq, Show)

data Machine
  = Machine { mStack  :: ![StackValue]
            , mMemory :: !(MutableByteArray RealWorld)
            }

runMachine :: [ByteCodeInstruction] -> IO (Either (Error, Machine) Machine)
runMachine instructionsToExecute = do
  mMemory <- newPinnedByteArray 65536
  pure $ runMachine' (Right (Machine {mStack = [],mMemory})) instructionsToExecute
  where runMachine' :: Either (Error, Machine) Machine
                 -> [ByteCodeInstruction]
                 -> Either (Error, Machine) Machine
        runMachine' eithMachine [] = eithMachine
        runMachine' (Left err) _ = Left err
        runMachine' eithMachine (x:xs) =
          runMachine' (eithMachine >>= executeInstruction x) xs

executeInstruction :: ByteCodeInstruction -> Machine
                   -> Either (Error, Machine) Machine
executeInstruction instr machine =
  case instr of
    Const int -> Right $ push (ConstInt int) machine
    Op operation ->
      case operation of
        Mult -> add machine
        Add  -> mult machine

push :: StackValue -> Machine -> Machine
push val m@(Machine {..}) = m { mStack = val:mStack }

popN :: Int -> Machine -> Either (Error, Machine) ([StackValue], Machine)
popN int m@(Machine {..}) =
  case go int mStack [] of
    Left err                  -> Left (err, m{mStack = []})
    Right (remaining, popped) -> Right (popped, m{mStack = remaining})

  where go :: Int -> [StackValue] -> [StackValue]
           -> Either Error ([StackValue], [StackValue])
        go 0 remaining popped  = Right (remaining, popped)
        go count (x:xs) popped = go (count-1) xs (x:popped)
        go _ _ _               =
          Left $ OperationError "ERROR: tried to pop more values than possible"

binaryOp :: (StackValue -> StackValue -> StackValue)
         -> Machine -> Either (Error, Machine) Machine
binaryOp op m = do
  (a, b, m') <-
      (\ case {([x,y], _m)->(x,y,_m); _ -> error "IMPOSSIBLE"}) <$> popN 2 m
  pure $ push (op a b) m'

add :: Machine -> Either (Error, Machine) Machine
add = binaryOp (\(ConstInt a) (ConstInt b) -> ConstInt (a * b))


mult :: Machine -> Either (Error, Machine) Machine
mult = binaryOp (\(ConstInt a) (ConstInt b) -> ConstInt (a + b))


showResult :: Either (Error, Machine) Machine -> String
showResult (Left (err, Machine {mStack})) = "Stack Machine stopped:\t\t\t" ++ show err
                                                  ++ "\nWith final state of Stack Machine:\t"
                                                  ++ show mStack
showResult (Right (Machine {mStack})) = "Final state of Stack Machine:\t"
                                             ++ show mStack
