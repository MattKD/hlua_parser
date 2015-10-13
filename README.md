# hlua_parser
Lua 5.2 parser in Haskell using Parsec

To test converting Lua source to the AST, run parser-test -i "Lua src", or parser-test file.lua.

Example:
```
parser-test -i "n = foo() + 3"
```
Prints:
```
Chunk [AssignStat [NameVar "n" []] [PExpExp (FnCallPExp (NameFnCall "foo" [FnCal
lTail [] (FnCallArgs (ExpArgs []))]) []) [ExpTail AddBinOp (NumberExp 3.0 [])]]]
 Nothing
```
