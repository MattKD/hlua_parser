-- Matt Donovan
-- Lua Compiler

import System.Environment
import Parser

main = do
  args <- getArgs
  if length args == 2 then 
    if args !! 0 == "-ast" then do
      ast <- parseFile $ args !! 1
      putStrLn $ show ast
    else do
      putStrLn "Incorrect number arguments"
      putStrLn "Usage: hluac [-ast] FILE.lua"
  else do
    putStrLn "Error only parser implemented. Use -ast option to print AST."
    putStrLn "Usage: hluac [-ast] FILE.lua"
     
