# hlua_parser
Lua 5.2 parser in Haskell using Parsec

To test converting Lua source to the AST, run parser-test -i "Lua src", or parser-test file.lua.

Example:
```
parser-test -i "n = foo() + 3 * 2"
```
Prints:
```
Chunk [AssignStat [NameVar "n" []] [BinExp AddBinOp (FnCallExp (NameFnCall "foo"
 [FnCallArgs [] (ExpArgs [])]) []) (BinExp MultBinOp (NumberExp 3.0) (NumberExp
2.0))]] Nothing
```
