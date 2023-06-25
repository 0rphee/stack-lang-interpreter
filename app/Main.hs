module Main (main) where

import Control.Monad.Trans.Except ( runExceptT )

import Data.Int
import Data.Primitive.Types

import Machine

putLn :: IO ()
putLn = putStrLn ""

-- Example code for the 'Machine'.
code :: [ByteCodeInstruction]
code = [ Const 2,
         Const 3,
         Const 4,
         Op Mult,
         Op Add
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
  runInterpreter code >>= (putStrLn . showResult)
  putLn
