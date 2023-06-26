module Main (main) where

import Data.Int
import Data.Primitive.Types
import Machine

putLn :: IO ()
putLn = putStrLn ""

-- Example code for the 'Machine'.
xAddr = 22

yAddr = 42

-- code :: [ByteCodeInstruction]
-- code =
--   [ Const 2
--   , Const xAddr
--   , Op Store
--   , Const 3
--   , Const yAddr
--   , Op Store
--   , Const xAddr
--   , Const xAddr
--   , Op Load
--   , Const yAddr
--   , Op Load
--   , Const 1
--   , Op Mult
--   , Op Add
--   , Op Store
--   ]

code =
  [ Const xAddr
  , Const 20
  , Op Store
  , Const xAddr
  , Op Load
  ]

showSize :: Prim a => a -> String -> String
showSize a name = "Size of " ++ name ++ "(bytes): " ++ show (sizeOf a)

main :: IO ()
main = do
  putLn
  putStrLn $ showSize (1 :: Int32) "Int32"
  putStrLn $ showSize (1 :: Int64) "Int64"
  putStrLn $ showSize (1 :: Float32) "Float32"
  putStrLn $ showSize (1 :: Float64) "Float64"
  runInterpreter code >>= showResult xAddr >>= putStrLn
  putLn
