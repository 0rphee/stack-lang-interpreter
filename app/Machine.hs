module Machine
  ( ByteCodeInstruction (..)
  , Error
  , Float32
  , Float64
  , Int32
  , Int64
  , Machine
  , MachineOperation (..)
  , StackValue
  , runInterpreter
  , showResult
  , showMemAddress
  )
where

import Control.Monad.Primitive (RealWorld)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Int
import Data.Primitive.ByteArray
import Debug.Trace (trace, traceShowId)

type Float32 = Float

type Float64 = Double

type VMemory = (MutableByteArray RealWorld)

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
  | Load
  | Store
  deriving (Eq, Show)

data Machine = Machine
  { mStack :: ![StackValue]
  , mMemory :: !VMemory
  }

type InterpM a = StateT Machine (ExceptT (Error, Machine) IO) a

showResult :: Int -> Either (Error, Machine) Machine -> IO String
showResult addr (Left (err, Machine {..})) = do
  addrStr <- showMemAddress addr mMemory
  pure
    ( "Stack Machine stopped:\t\t\t"
        ++ show err
        ++ "\nWith final state of Stack Machine:\t"
        ++ show mStack
        ++ "\n"
        ++ addrStr
    )
showResult addr (Right (Machine {..})) = do
  addrStr <- showMemAddress addr mMemory
  pure
    ( "Final state of Stack Machine:\t"
        ++ show mStack
        ++ "\n"
        ++ addrStr
    )

showMemAddress :: Int -> VMemory -> IO String
showMemAddress addr mMemory = do
  (v :: Double) <- readByteArray mMemory addr
  pure $ "The value at address " ++ show addr ++ " is: " ++ show v

runInterpreter :: [ByteCodeInstruction] -> IO (Either (Error, Machine) Machine)
runInterpreter instructionsToExecute = do
  mMemory <- newPinnedByteArray 65536
  let machine = Machine {mStack = [], mMemory}
  runExceptT (execStateT (eval instructionsToExecute) machine)

eval :: [ByteCodeInstruction] -> InterpM ()
eval [] = pure ()
eval (Const val : xs) = do
  pushs (ConstInt val)
  eval xs
eval (Op op : xs) = do
  case op of
    Mult -> mults
    Add -> adds
    Load -> loads
    Store -> stores
  eval xs

loads :: InterpM ()
loads = do
  (ConstInt addr) <- pops
  mMemory <- gets mMemory
  (res :: Int) <- lift $ readByteArray mMemory addr
  pushs (ConstInt res)

stores :: InterpM ()
stores = do
  (ConstInt valToStore) <- pops
  (ConstInt addrToStore) <- pops
  mMemory <- gets mMemory
  lift $ writeByteArray mMemory addrToStore valToStore

adds :: InterpM ()
adds = binOp (+)

mults :: InterpM ()
mults = binOp (*)

binOp :: (Int -> Int -> Int) -> InterpM ()
binOp f = do
  (ConstInt val1) <- pops
  (ConstInt val2) <- pops
  pushs $ ConstInt (f val1 val2)

pops :: InterpM StackValue
pops = do
  stack <- gets mStack
  case stack of
    [] -> do
      st <- get
      lift $ throwE (OperationError "ERROR: tried to pop more values than possible", st)
    (x : xs) -> do
      modify' (\m@(Machine {}) -> m {mStack = xs})
      pure x

pushs :: StackValue -> InterpM ()
pushs val = modify' primPush
  where
    primPush :: Machine -> Machine
    primPush m@(Machine {..}) = m {mStack = val : mStack}
