-- Lua 5.2 Parser using Parsec
-- Matt Donovan

module Parser
( parseFile
, parseString
, Chunk(..)
, Stat(..)
, RetStat(..)
, Var(..)
, Exp(..)
, ExpTail(..)
, TblLookup(..)
, FnCall(..)
, FnCallTail(..)
, FnCallArgs(..)
, Args(..)
, FuncBody(..)
, ParList(..)
, Field(..)
, BinOp(..)
, UnOp(..)
) where
  
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Maybe (isNothing)

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

-- Lua AST data types

data Chunk = Chunk [Stat] (Maybe RetStat) 
             deriving (Show)

data Stat = AssignStat [Var] [Exp] | FnCallStat FnCall | LabelStat String |
            BreakStat | GotoStat String | DoStat Chunk | WhileStat Exp Chunk |
            RepeatStat Chunk Exp | 
            IfStat Exp Chunk [(Exp, Chunk)] (Maybe Chunk) |
            ForStat String Exp Exp (Maybe Exp) Chunk |
            ForInStat [String] [Exp] Chunk |
            FnDecStat [String] (Maybe String) FuncBody |
            LocalFnStat String FuncBody | LocalVarStat [String] [Exp]
            deriving (Show)

data RetStat = RetStat [Exp] deriving (Show)

data Var = NameVar String [TblLookup] | 
           FnCallVar FnCall [TblLookup] |
           ExpVar Exp [TblLookup]
           deriving (Show)

data Exp = NilExp [ExpTail] | FalseExp [ExpTail] | TrueExp [ExpTail] | 
           NumberExp Double [ExpTail] | StringExp String [ExpTail] | 
           VarArgExp [ExpTail] | FuncExp FuncBody [ExpTail] | 
           NameExp String [TblLookup] [ExpTail] | 
           FnCallExp FnCall [TblLookup] [ExpTail] |
           ParenExp Exp [TblLookup] [ExpTail] |
           TblCtorExp [Field] [ExpTail] | UnOpExp UnOp Exp [ExpTail] 
           deriving (Show)

data ExpTail = ExpTail BinOp Exp
               deriving (Show)

data TblLookup = NameTblLookup String | ExpTblLookup Exp
                 deriving (Show)

data FnCall = NameFnCall String [FnCallTail] |
              ExpFnCall Exp [FnCallTail]
              deriving (Show)

data FnCallTail = FnCallTail [TblLookup] FnCallArgs
                  deriving (Show)

data FnCallArgs = FnCallArgs Args | TblFnCallArgs String Args 
                  deriving (Show)

data Args = ExpArgs [Exp] | TblCtorArgs [Field] | StrArgs String
            deriving (Show)

data FuncBody = FuncBody ParList Chunk deriving (Show)

-- List of param names and whether there's variable number of args
data ParList = ParList [String] Bool deriving (Show)

data Field = ExpField Exp | ExpExpField Exp Exp | NameExpField String Exp
             deriving (Show)

data BinOp = AddBinOp | SubBinOp | MultBinOp | DivBinOp | ExpBinOp | ModBinOp | 
             ConcatBinOp | LTBinOp | LTEBinOp | GTBinOp | GTEBinOp |
             EqBinOp | NEqBinOp | AndBinOp | OrBinOp
             deriving (Show)

data UnOp = NegateUnOp | NotUnOp | HashUnOp
            deriving (Show)

-- Lexical token definitions 
languageDef =
  emptyDef { Token.commentStart    = ""
           , Token.commentEnd      = ""
           , Token.commentLine     = "--"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.opStart         = oneOf "+-*/^%.<>=~#:"
           , Token.opLetter        = oneOf "=:."
           , Token.reservedNames   = ["and", "break", "do", "else", "elseif",
                                      "end", "false", "for", "function",
                                      "goto", "if", "in", "local", "nil", 
                                      "not", "or", "repeat", "return", "then",
                                      "true", "until", "while"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "^", "%", ".."
                                     ,"<", "<=", ">", ">=", "==", "~="
                                     ,"#", "=", "...", ".", ":"
                                     ]
           }

lexer         = Token.makeTokenParser languageDef
identifier    = Token.identifier      lexer 
reserved      = Token.reserved        lexer 
reservedOp    = Token.reservedOp      lexer 
parens        = Token.parens          lexer 
brackets      = Token.brackets        lexer 
braces        = Token.braces          lexer 
integer       = Token.integer         lexer 
float         = Token.float           lexer 
semi          = Token.semi            lexer 
whiteSpace    = Token.whiteSpace      lexer 
comma         = Token.comma           lexer
stringLiteral = Token.stringLiteral   lexer

-- Parser functions

whileParser :: Parser Chunk
whileParser = whiteSpace >> (do { c <- parseChunk; eof; return c})

parseNumber :: Parser Double
parseNumber = try float <|>
              do { i <- integer; return $ fromIntegral i }

parseChunk :: Parser Chunk
parseChunk = do
  stats <- (endBy parseStat (optional semi))
  retStat <- optionMaybe parseRetStat
  return $ Chunk stats retStat

parseStat :: Parser Stat
parseStat = try assignStat <|> fnCallStat <|> labelStat <|> breakStat <|> 
            gotoStat <|> doStat <|> whileStat <|> repeatStat <|> ifStat <|>
            (try forStat) <|> forInStat <|> funcDecStat <|> 
            (try localFuncStat) <|> localVarStat
  where
    assignStat = do 
      vars <- parseVarList
      reservedOp "="
      exps <- parseExpList
      return $ AssignStat vars exps
    fnCallStat = do
      f <- parseFnCall
      return $ FnCallStat f
    labelStat = do
      reservedOp "::"
      id <- identifier
      reservedOp "::"
      return $ LabelStat id
    breakStat = do
      reserved "break" 
      return BreakStat
    gotoStat = do
      reserved "goto"
      id <- identifier
      return $ GotoStat id
    doStat = do
      reserved "do"
      block <- parseChunk
      reserved "end"
      return $ DoStat block
    whileStat = do
      reserved "while"
      exp <- parseExp
      reserved "do"
      block <- parseChunk
      reserved "end"
      return $ WhileStat exp block
    repeatStat = do
      reserved "repeat"
      block <- parseChunk
      reserved "until"
      exp <- parseExp
      return $ RepeatStat block exp
    ifStat = do
      reserved "if"
      exp <- parseExp
      reserved "then"
      block <- parseChunk
      elifs <- many (do 
        reserved "elseif"
        e <- parseExp
        reserved "then"
        b <- parseChunk
        return (e,b))
      els <- optionMaybe (do {reserved "else"; parseChunk})
      reserved "end"
      return $ IfStat exp block elifs els
    forStat = do
      reserved "for"
      id <- identifier
      reservedOp "="
      exp1 <- parseExp
      comma
      exp2 <- parseExp
      exp3 <- optionMaybe $ do {comma; parseExp}
      reserved "do"
      block <- parseChunk
      reserved "end"
      return $ ForStat id exp1 exp2 exp3 block
    forInStat = do
      reserved "for"
      ids <- parseNameList
      reserved "in"
      exps <- parseExpList
      reserved "do"
      block <- parseChunk
      reserved "end"
      return $ ForInStat ids exps block
    funcDecStat = do
      reserved "function"
      ids <- sepBy1 identifier (reservedOp ".")
      id2 <- optionMaybe $ do {reservedOp ":"; identifier}
      funcBody <- parseFuncBody
      return $ FnDecStat ids id2 funcBody
    localFuncStat = do
      reserved "local"
      reserved "function"
      id <- identifier
      funcBody <- parseFuncBody
      return $ LocalFnStat id funcBody
    localVarStat = do
      reserved "local"
      ids <- parseNameList
      exps <- option [] $ do {reservedOp "="; parseExpList}
      return $ LocalVarStat ids exps
   
parseRetStat = do
  reserved "return"
  exps <- option [] parseExpList
  option "" semi
  return $ RetStat exps

parseVarList :: Parser [Var]
parseVarList = sepBy1 parseVar comma

parseVar :: Parser Var
parseVar = try fnCallVar <|> expVar <|> nameVar 
  where
    fnCallVar = do
      fnCall <- parseFnCall
      tblLookups <- many1 parseTblLookup
      return $ FnCallVar fnCall tblLookups
    expVar = do
      exp <- parens parseExp
      tblLookups <- many1 parseTblLookup
      return $ ExpVar exp tblLookups
    nameVar = do
      id <- identifier
      tblLookups <- many parseTblLookup
      return $ NameVar id tblLookups

parseNameList :: Parser [String]
parseNameList = sepBy1 identifier comma

parseExpList :: Parser [Exp]
parseExpList = sepBy1 parseExp comma

parseExp :: Parser Exp
parseExp = nilExp <|> falseExp <|> trueExp <|> strExp <|> varArgExp <|>
           fnDefExp <|> (try fnCallExp) <|> nameExp <|> parenExp <|> 
           tblCtorExp <|> (try numExp) <|> unopExp
  where
    nilExp = do 
      reserved "nil" 
      expTail <- many parseExpTail
      return $ NilExp expTail
    falseExp = do
      reserved "false" 
      expTail <- many parseExpTail
      return $ FalseExp expTail
    trueExp = do
      reserved "true"
      expTail <- many parseExpTail
      return $ TrueExp expTail
    numExp = do
      n <- parseNumber
      expTail <- many parseExpTail
      return $ NumberExp n expTail
    strExp = do
      s <- stringLiteral
      expTail <- many parseExpTail
      return $ StringExp s expTail
    varArgExp = do 
      reservedOp "..."
      expTail <- many parseExpTail
      return $ VarArgExp expTail
    fnDefExp = do
      reserved "function"
      funcBody <- parseFuncBody
      expTail <- many parseExpTail
      return $ FuncExp funcBody expTail
    fnCallExp = do
      f <- parseFnCall
      tblLookups <- many parseTblLookup
      expTail <- many parseExpTail
      return $ FnCallExp f tblLookups expTail
    nameExp = do
      id <- identifier
      tblLookups <- many parseTblLookup
      expTail <- many parseExpTail
      return $ NameExp id tblLookups expTail
    parenExp = do
      exp <- parens parseExp
      tblLookups <- many parseTblLookup
      expTail <- many parseExpTail
      return $ ParenExp exp tblLookups expTail 
    tblCtorExp = do
      fieldlist <- parseTblCtor
      expTail <- many parseExpTail
      return $ TblCtorExp fieldlist expTail 
    unopExp = do
      unOp <- parseUnOp 
      exp <- parseExp
      expTail <- many parseExpTail
      return $ UnOpExp unOp exp expTail

parseExpTail :: Parser ExpTail
parseExpTail = do
  binOp <- parseBinOp
  exp <- parseExp
  return $ ExpTail binOp exp

parseTblLookup :: Parser TblLookup
parseTblLookup = expTblLookup <|> nameTblLookup 
  where
    expTblLookup = do
      exp <- brackets parseExp
      return $ ExpTblLookup exp
    nameTblLookup = do
      reservedOp "."
      id <- identifier
      return $ NameTblLookup id
  
parseFnCall :: Parser FnCall
parseFnCall = nameFnCall <|> expFnCall 
  where
    nameFnCall = do
      id <- identifier
      fnCallTail <- many1 $ try parseFnCallTail
      return $ NameFnCall id fnCallTail
    expFnCall = do
      exp <- parens parseExp
      fnCallTail <- many1 $ try parseFnCallTail
      return $ ExpFnCall exp fnCallTail

parseFnCallTail :: Parser FnCallTail
parseFnCallTail = do
  tblLookups <- many parseTblLookup
  fnCallArgs <- parseFnCallArgs
  return $ FnCallTail tblLookups fnCallArgs
  
parseFnCallArgs :: Parser FnCallArgs
parseFnCallArgs = args <|> tblArgs 
  where
    args = do
      a <- parseArgs
      return $ FnCallArgs a 
    tblArgs = do
      reservedOp ":"
      id <- identifier
      a <- parseArgs
      return $ TblFnCallArgs id a    
 
parseArgs :: Parser Args
parseArgs = expArgs <|> tblCtorArgs <|> strArgs 
  where
    expArgs = do
      expList <- parens (option [] parseExpList)
      return $ ExpArgs expList
    tblCtorArgs = do
      fields <- parseTblCtor
      return $ TblCtorArgs fields
    strArgs = do
      str <- stringLiteral
      return $ StrArgs str

parseFuncBody :: Parser FuncBody
parseFuncBody = do
  parlist <- parens (option (ParList [] False) parseParList)
  block <- parseChunk
  reserved "end"
  return $ FuncBody parlist block

parseParList :: Parser ParList
parseParList = nameParList <|> varArgParList 
  where
    nameParList = do
      id <- identifier
      ids <- many $ try (comma >> identifier)
      let params = id:ids
      varArgStr <- optionMaybe (comma >> reservedOp "...")
      let hasVarArgs = if isNothing varArgStr then False else True
      return $ ParList params hasVarArgs
    varArgParList = do
      reservedOp "..." 
      return $ ParList [] True

parseTblCtor :: Parser [Field]
parseTblCtor = braces $ sepEndBy parseField (comma <|> semi)

parseField :: Parser Field
parseField = expExpField <|> (try nameExpField) <|> expField 
  where
    expExpField = do
      e1 <- brackets parseExp
      reservedOp "="
      e2 <- parseExp
      return $ ExpExpField e1 e2 
    nameExpField = do
      i <- identifier
      reservedOp "="
      e <- parseExp
      return $ NameExpField i e 
    expField = do
      e <- parseExp
      return $ ExpField e

parseBinOp :: Parser BinOp
parseBinOp = addBinOp <|> subBinOp <|> multBinOp <|> divBinOp <|> 
             expBinOp <|> modBinOp <|> concatBinOp <|> ltBinOp <|>
             lteBinOp <|> gtBinOp <|> gteBinOp <|> eqBinOp <|> neqBinOp <|>
             andBinOp <|> orBinOp 
  where
    addBinOp = reservedOp "+" >> return AddBinOp
    subBinOp = reservedOp "-" >> return SubBinOp
    multBinOp = reservedOp "*" >> return MultBinOp
    divBinOp = reservedOp "/" >> return DivBinOp
    expBinOp = reservedOp "^" >> return ExpBinOp
    modBinOp = reservedOp "%" >> return ModBinOp
    concatBinOp = reservedOp ".." >> return ConcatBinOp
    ltBinOp = reservedOp "<" >> return LTBinOp
    lteBinOp = reservedOp "<=" >> return LTEBinOp
    gtBinOp = reservedOp ">" >> return GTBinOp
    gteBinOp = reservedOp ">=" >> return GTEBinOp
    eqBinOp = reservedOp "==" >> return EqBinOp
    neqBinOp = reservedOp "~=" >> return NEqBinOp
    andBinOp = reserved "and" >> return AndBinOp
    orBinOp = reserved "or" >> return OrBinOp

parseUnOp :: Parser UnOp
parseUnOp = negateOp <|> notOp <|> hashOp 
  where
    negateOp = reservedOp "-" >> return NegateUnOp
    notOp = reserved "not" >> return NotUnOp
    hashOp = reservedOp "#" >> return HashUnOp

