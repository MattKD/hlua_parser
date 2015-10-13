import System.Environment
import Parser

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then do
      ast <- parseFile $ args !! 0
      putStrLn $ show ast
  else if length args == 2 then 
    if args !! 0 == "-i" then do
      let lua_str = args !! 1
      let ast = parseString lua_str
      putStrLn $ show ast
    else printUsage
  else printUsage

printUsage :: IO ()
printUsage = do
  putStrLn "Usage: parser-test (-i \"LUA_STR\" | FILE.lua)"
     
