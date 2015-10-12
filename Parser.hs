-- Matt Donovan
-- Lua Parser

module Parser where
  
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Chunk = Chunk [Stat] (Maybe LastStat) 
             deriving (Show)

data Stat = Assign Var Exp | FunCallStat FunCall |
            IfStat Exp Chunk | WhileStat Exp Chunk
            deriving (Show)

data LastStat = Return [Exp] | Break
                deriving (Show)

data Var = Var String [VarSuffix]
           deriving (Show)

data VarSuffix = TblExpLookup Exp | TblNameLookup String |
                 FunCallArgs [Exp]
                 deriving (Show)

data PExp = PExpVar Var | PExpFunCall FunCall
            deriving (Show)

data FunCall = FunCall String [VarSuffix]
               deriving (Show)

data Field = ExpField Exp | ExpExpField Exp Exp | NameExpField String Exp
             deriving (Show)

data Param = Param String
             deriving (Show)

data Exp = Nil | LFalse | LTrue | Number Double |
           LString String | BinExp BinOp Exp Exp | Neg Exp |
           ExpPExp PExp | TableCtor [Field] | FuncExp [Param] Chunk
           deriving (Show)

data BinOp = Add | Subtract | Multiply | Divide | LessThan | GreaterThan |
             EqualTo | NotEqualTo | And | Or
             deriving (Show)

languageDef =
  emptyDef { Token.commentStart    = ""
           , Token.commentEnd      = ""
           , Token.commentLine     = "--"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = ["and", "break", "do", "else", "elseif",
                                      "end", "false", "for", "function",
                                      "if", "in", "local", "nil", "not",
                                      "or", "repeat", "return", "then",
                                      "true", "until", "while"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "^", "%", ".."
                                     ,"<", "<=", ">", ">=", "==", "~="
                                     ,"and", "or", "not", "#", "="
                                     ]
           }

lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer 
reserved   = Token.reserved   lexer 
reservedOp = Token.reservedOp lexer 
parens     = Token.parens     lexer 
integer    = Token.integer    lexer 
float    = Token.float    lexer 
semi       = Token.semi       lexer 
whiteSpace = Token.whiteSpace lexer 
comma      = Token.comma      lexer
stringLiteral = Token.stringLiteral lexer

whileParser :: Parser Chunk
whileParser = whiteSpace >> (do { c <- chunk; eof; return c})

number = try float
       <|> do { i <- integer; return $ fromIntegral i }

chunk :: Parser Chunk
chunk = do
  stats <- (endBy1 stat (optional semi))
  rtn <- optionMaybe (do 
    r <- laststat 
    optional semi
    return r)
  return $ Chunk stats rtn

stat :: Parser Stat
stat = try assignStat <|> funCallStat
     <|> ifStat <|> whileStat
 
assignStat :: Parser Stat
assignStat = do 
  v <- var
  reservedOp "="
  e <- binExpr
  return $ Assign v e

funCallStat = do
  f <- funcall
  return $ FunCallStat f

ifStat = do
  reserved "if"
  e <- binExpr
  reserved "then"
  block <- chunk
  reserved "end"
  return $ IfStat e block

whileStat = do
  reserved "while"
  e <- binExpr
  reserved "do"
  block <- chunk
  reserved "end"
  return $ WhileStat e block

laststat = do
  reserved "return"
  exps <- option [] explist
  return $ Return exps

--varlist = sepBy1 identifier comma

var = do
  name <- identifier
  suffixes <- var1
  return $ Var name suffixes 

  where
    var1 = var2 <|> var3 <|> (return [])

    var2 = 
      do
        char '[' >> whiteSpace
        e <- binExpr
        char ']' >> whiteSpace
        vs <- var1
        return $ (TblExpLookup e) : vs
      <|> do
        char '.' >> whiteSpace
        name <- identifier
        vs <- var1
        return $ (TblNameLookup name) : vs
    var3 = 
      do
        exps <- args 
        vs <- var4 
        return $ (FunCallArgs exps) : vs 
    var4 = var2 <|> var3
        

namelist = sepBy1 identifier comma

explist = sepBy1 binExpr comma

expr :: Parser Exp
expr = try (reserved "nil" >> (return Nil))
    <|> try (reserved "false" >> (return LFalse))
    <|> try (reserved "true" >> (return LTrue))
    <|> try (do { n <- number; return $ Number n })
    <|> (do { s <- stringLiteral; return $ LString s })
    <|> (do { p <- pexp; return $ ExpPExp p })
    <|> tableCtor
    <|> funcExp
 
pexp = 
  try (do
    f <- funcall
    return $ PExpFunCall f)
  <|> (do
    v <- var
    return $ PExpVar v)

funcall = do
  name <- identifier
  suffixes <- fc1
  return $ FunCall name suffixes 

  where
    fc1 = fc2 <|> fc3 
    fc2 = 
      do
        char '[' >> whiteSpace
        e <- binExpr
        char ']' >> whiteSpace
        suffixes <- fc1
        return $ (TblExpLookup e) : suffixes
      <|> do
        char '.' >> whiteSpace
        name <- identifier
        suffixes <- fc1
        return $ (TblNameLookup name) : suffixes
    fc3 = 
      do
        exps <- args 
        suffixes <- fc4
        return $ (FunCallArgs exps) : suffixes
    fc4 = fc1 <|> (return [])

args = parens (option [] explist)
  
funcExp = do
  reserved "function"
  names <- parens namelist 
  let params = map (\n -> Param n) names
  block <- chunk
  reserved "end"
  return $ FuncExp params block

tableCtor = do
  char '{'
  whiteSpace
  f <- sepBy field comma
  char '}'
  whiteSpace
  return $ TableCtor f

field = expExpField
      <|> try nameExpField
      <|> expField

expExpField = do
  char '['
  whiteSpace
  e1 <- binExpr
  char ']'
  whiteSpace
  char '='
  whiteSpace
  e2 <- binExpr
  return $ ExpExpField e1 e2 

nameExpField = do
  i <- identifier
  reservedOp "="
  e <- binExpr
  return $ NameExpField i e 
  
expField = do
  e <- binExpr
  return $ ExpField e

--
-- functions handling binary expresiion parsing and operator precedence
-- Use binExpr wherever expr should be used in Lua's grammar
--
binExpr = buildExpressionParser aOperators binTerm

binTerm =  parens binExpr
        <|> expr

aOperators = 
  [[Prefix (reservedOp "-" >> return (Neg))],

   [Infix  (reservedOp "*" >> return (BinExp Multiply)) AssocLeft,
    Infix  (reservedOp "/" >> return (BinExp Divide)) AssocLeft],

   [Infix  (reservedOp "+" >> return (BinExp Add)) AssocLeft,
    Infix  (reservedOp "-" >> return (BinExp Subtract)) AssocLeft],

   [Infix  (reservedOp ">" >> return (BinExp GreaterThan)) AssocLeft,
    Infix  (reservedOp "<" >> return (BinExp LessThan)) AssocLeft,
    Infix  (reservedOp "~=" >> return (BinExp NotEqualTo)) AssocLeft,
    Infix  (reservedOp "==" >> return (BinExp EqualTo)) AssocLeft],
     
   [Infix  (reservedOp "and" >> return (BinExp And)) AssocLeft,
    Infix  (reservedOp "or" >> return (BinExp Or)) AssocLeft]
  ]


parseFile :: String -> IO Chunk
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

parseString :: String -> Chunk
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

