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
  , runMachine
  , showResult
  ) where
import Control.Monad.Primitive          ( RealWorld )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict

import Data.Int
import Data.Primitive                   ( MutableByteArray )
import Data.Primitive.ByteArray


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

data Machine
  = Machine { mStack  :: ![StackValue]
            , mMemory :: !VMemory
            }

type InterpM a = StateT Machine (ExceptT (Error, Machine) IO) a

runMachine :: [ByteCodeInstruction] -> ExceptT (Error, Machine) IO Machine
runMachine instructionsToExecute = do
  mMemory <- lift $ newPinnedByteArray 65536
  runMachine' instructionsToExecute $ Machine {mStack = [],mMemory}
  where runMachine' :: [ByteCodeInstruction]
                     -> Machine ->  ExceptT (Error, Machine) IO Machine
        runMachine' [] machine  = pure machine
        runMachine' (x:xs) machine =
          executeInstruction x machine >>= runMachine' xs


runInterpreter :: [ByteCodeInstruction] -> IO (Either (Error, Machine) Machine)
runInterpreter instructionsToExecute= do
  mMemory <- newPinnedByteArray 65536
  let machine = Machine {mStack=[], mMemory}
  runExceptT (execStateT (eval instructionsToExecute) machine)


eval :: [ByteCodeInstruction] -> InterpM ()
eval [] = pure ()
eval (Const val:xs) = do
  pushs (ConstInt val)
  eval xs
eval (Op op:xs) = do
  case op of
    Mult  -> mults
    Add   -> adds
    Load  -> loads
    Store -> stores
  eval xs


loads :: InterpM ()
loads = do
  (ConstInt addr) <- pops
  mMemory <- gets mMemory
  (res::Int) <- lift $ readByteArray mMemory addr
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
     (x:xs) -> do
       modify' (\m@(Machine{})->m{mStack=xs})
       pure x

pushs :: StackValue -> InterpM ()
pushs val = modify' primPush
  where primPush :: Machine -> Machine
        primPush m@(Machine {..}) = m{mStack = val:mStack}




executeInstruction :: ByteCodeInstruction -> Machine
                   -> ExceptT (Error, Machine) IO Machine
executeInstruction instr machine =
  case instr of
    Const int -> pure $ push (ConstInt int) machine
    Op operation ->
      case operation of
        Mult -> except $ add machine
        Add  -> except $ mult machine
        Load -> load machine

push :: StackValue -> Machine -> Machine
push val m@(Machine {..}) = m { mStack = val:mStack }

pop :: Machine -> Either (Error, Machine) (StackValue, Machine)
pop m@(Machine {..}) =
  case take 1 mStack of
    [] -> Left (OperationError "ERROR: tried to pop more values than possible"
                  , m)
    (x:xs) -> Right (x, m{mStack = xs})



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

-- | Load from memory to the top of the stack
load :: Machine -> ExceptT (Error, Machine) IO Machine
load m@(Machine{..}) = do
  (ConstInt address, m') <- except $ pop m
  (res :: Int) <- lift $ readByteArray mMemory address
  pure $ push (ConstInt res) m'

store :: Machine -> ExceptT (Error, Machine) IO Machine
store m = do
  (ConstInt var, m') <- except $ pop m
  (ConstInt address, m''@(Machine{..})) <- except $ pop m'
  lift $ writeByteArray mMemory address var
  pure m''



showResult :: Either (Error, Machine) Machine -> String
showResult (Left (err, Machine {mStack})) = "Stack Machine stopped:\t\t\t" ++ show err
                                                  ++ "\nWith final state of Stack Machine:\t"
                                                  ++ show mStack
showResult (Right (Machine {mStack})) = "Final state of Stack Machine:\t"
                                             ++ show mStack

