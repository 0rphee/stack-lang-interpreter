module Main (main) where

import Lib 

putLn :: IO ()
putLn = putStrLn ""

-- Example code for the 'Machine'.
code :: [Instruction]
code = [ Const 2,
         Const 3,
         Const 4,
         Op Mult,
         Op Add
       ]

main :: IO ()
main = do
  putLn
  putStrLn $ showResult $ execute code
  putLn
