module Main (main) where

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

main :: IO ()
main = do
  putLn
  putStrLn . showResult =<< runMachine code
  putLn
