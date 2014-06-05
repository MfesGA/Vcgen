module Parser where

import           Control.Applicative           hiding (many, (<|>))
import           Control.Monad
import           Data.Functor.Identity
import           Syntax
import           Text.Parsec.Expr              as Pex
import           Text.Parsec.Prim      as Prim hiding(try)
import           Text.ParserCombinators.Parsec
import           AuxParsers as Aux

teste :: IO ()
teste =  forever $ 
    getLine >>= readFile >>= parseTest parseSource.clearS >>= print

parseFile :: String -> IO (Either String Source)
parseFile str = do
    let result = Prim.parse parseSource "" $ clearS str
    case result of
      Left err -> return $ Left $ clearS str ++ " | " ++ show err
      Right val -> return $ Right val


-- Parse Source

parseSource :: ParsecT String u Identity Source
parseSource = do
    preC <- parsePre
    exprs <- many $ try parseExpr
    posC <- parsePos
    return $ Source preC posC exprs

-- ParseExpr

parseExpr :: ParsecT String u Identity Expr
parseExpr = parseIf <|> parseWhile  <|> parseExprAlloc <|> parseExprAssign


-- parseIf
parseIf :: ParsecT String u Identity Expr
parseIf = do
    _ <- pIf
    _ <- parO
    cond <- parseLogExp
    _ <- parC
    _ <- bracketO
    exprs1 <- many parseExpr
    _ <- bracketC
    _ <- pElse
    _ <- bracketO
    exprs2 <- many parseExpr
    _ <- bracketC
    return $ ExprIf cond exprs1 exprs2

-- parse while
parseWhile :: ParsecT String u Identity Expr
parseWhile = do
    _ <- while
    _ <- parO
    cond <-  parseLogExp
    _ <- parC
    _ <- bracketO
    pinv <- parseInv
    exprs <- many parseExpr
    _ <- bracketC
    return $ ExprWhile cond pinv exprs


-- parse Invariant

parseInv :: ParsecT String u Identity LogExp
parseInv = inv *> parseLogExp <* sep

-- parse Pre condition
parsePre :: ParsecT String u Identity LogExp
parsePre = pre *> parseLogExp <* sep


-- Parse post condition
parsePos :: ParsecT String u Identity LogExp
parsePos = pos *> parseLogExp <* sep


-- Parse Alloc

parseExprAlloc :: ParsecT String u Identity Expr
parseExprAlloc = liftM ExprAlloc parseAlloc

parseAlloc :: ParsecT String u Identity Alloc
parseAlloc = (try parseANArray <|> parseNVar ) <* sep

parseNVar :: ParsecT String u Identity Alloc
parseNVar = pVar *> liftM NVar symbol

parseANArray :: ParsecT String u Identity Alloc
parseANArray = do
    _ <- pVar
    name <- symbol
    _ <- squareO
    size <- integer
    _ <- squareC
    return $ ANArray name size

-- ParseAssign

parseExprAssign :: ParsecT String u Identity Expr
parseExprAssign = liftM ExprAssign parseAssign

parseAssign :: ParsecT String u Identity Assign
parseAssign =
            ( try parseAssignVar 
          <|> try parseAssignIntArray
          <|> parseAssignVarArray )
          <* sep
parseAssignVar :: ParsecT String u Identity Assign
parseAssignVar = do
    var <- symbol
    _ <- eq
    aexp <- parseAExp
    return $ AssignVar var aexp


parseAssignIntArray :: ParsecT String u Identity Assign
parseAssignIntArray = do
    var <- symbol
    _ <- squareO
    position <- integer
    _ <- squareC
    _ <- eq
    aexp <- parseAExp
    return $  AssignIntArray var position aexp


parseAssignVarArray :: ParsecT String u Identity Assign
parseAssignVarArray = do
    var <- symbol
    _ <- squareO
    position <- symbol
    _ <- squareC
    _ <- eq
    aexp <- parseAExp
    return $  AssignVarArray var position aexp

-- Parse LogExp

parseLogExp :: ParsecT String u Identity LogExp
parseLogExp = buildExpressionParser logExpTable logExpFactor



logExpTable :: [[Operator String u Identity LogExp]]
logExpTable  = [ [ Prefix (pNot >> return Not) ]
               , [ Infix (liftM LogBin parseAnd) AssocLeft]
               , [ Infix (liftM LogBin parseOr) AssocLeft]
               , [ Infix (liftM LogBin parseImp) AssocLeft]
               , [ Prefix parseForall, Prefix parseExists]

               ]


logExpFactor :: ParsecT String u Identity LogExp
logExpFactor = parseParLogExp <|> parseBConst <|> parseIneBin


parseParLogExp :: ParsecT String u Identity LogExp
parseParLogExp = parO*> parseLogExp <* parC


parseIneBin :: ParsecT String u Identity LogExp
parseIneBin = do
    exp1 <- parseAExp
    ineOp <- parseIneOp
    exp2 <- parseAExp
    return $ IneBin ineOp exp1 exp2




-- parse IneOp
parseIneOp :: ParsecT String u Identity IneOp
parseIneOp = try parseEqual
         <|> try parseLeq
         <|> try parseGeq
         <|> try parseDiff
         <|> try parseLt
         <|> parseGt


parseEqual :: ParsecT String u Identity IneOp
parseEqual = equal *> return Equal

parseDiff :: ParsecT String u Identity IneOp
parseDiff = diff *> return Diff

parseLt :: ParsecT String u Identity IneOp
parseLt = lt *> return Lt

parseGt :: ParsecT String u Identity IneOp
parseGt = gt *> return Gt

parseLeq :: ParsecT String u Identity IneOp
parseLeq = leq *> return Leq

parseGeq :: ParsecT String u Identity IneOp
parseGeq = geq *> return Geq


-- parse LogOp

parseImp :: ParsecT String u Identity LogOp
parseImp = pImp *> return Imp

parseAnd :: ParsecT String u Identity LogOp
parseAnd = pAnd *> return And

parseOr :: ParsecT String u Identity LogOp
parseOr = pOr *> return Or

parseForall :: ParsecT String u Identity (LogExp -> LogExp)
parseForall = do
    _ <- forall
    _ <- char ':'
    var <- symbol
    _ <- char ','
    return $ Forall  var

parseExists :: ParsecT String u Identity (LogExp -> LogExp)
parseExists = do
    _ <- exists
    _ <- char ':'
    var <- symbol
    _ <- char ','
    return $ Exists  var


-- Parse Boolean

parseBConst :: ParsecT String u Identity LogExp
parseBConst = liftM BConst parseBool

parseBool :: ParsecT String u Identity Bool
parseBool = parseTrue <|> parseFalse

parseTrue :: ParsecT String u Identity Bool
parseTrue =  true *> return True

parseFalse :: ParsecT String u Identity Bool
parseFalse = false *> return False



--- Parse AExp

parseAExp :: ParsecT String u Identity AExp
parseAExp = buildExpressionParser aExpTable aExpFactor

aExpTable :: [[Operator String u Identity AExp]]
aExpTable = [ [ Infix (liftM AExp parseMul ) AssocLeft
              , Infix (liftM AExp parseDiv) AssocLeft]
            , [ Infix (liftM AExp parseSum) AssocLeft
              , Infix (liftM AExp parseSub) AssocLeft]
            ]
aExpFactor :: ParsecT String u Identity AExp
aExpFactor = parseExpPar <|> parseSAValue

parseExpPar :: ParsecT String u Identity AExp
parseExpPar = parO *> parseAExp <* parC

parseSAValue :: ParsecT String u Identity AExp
parseSAValue= liftM SAValue parseVarValue



-- Parse AOp

parseSum :: ParsecT String u Identity AOp
parseSum = Aux.sum *> return Add

parseSub :: ParsecT String u Identity AOp
parseSub = sub *> return Sub

parseMul :: ParsecT String u Identity AOp
parseMul = mul *> return Mul

parseDiv :: ParsecT String u Identity AOp
parseDiv = Aux.div *> return Div

parseMod :: ParsecT String u Identity AOp
parseMod = Aux.mod *> return Mod




-- Function that parses a VarValue
parseVarValue :: ParsecT String u Identity VarValue
parseVarValue = parseVNum 
            <|> try parseVIArray 
            <|> try parseVVArray
            <|> try parseNArray
            <|> parseVVar

-- Functions to auxiliate the Parsing of  a VarValue

parseVNum :: ParsecT String u Identity VarValue
parseVNum = liftM VVNum integer

parseVVar :: ParsecT String u Identity VarValue
parseVVar = liftM VVar symbol

parseVIArray :: ParsecT String u Identity VarValue
parseVIArray = do
    name <- symbol
    _ <- squareO
    position <- integer
    _ <- squareC
    return $ VIArray name position

parseVVArray :: ParsecT String u Identity VarValue
parseVVArray = do
    name <- symbol
    _ <- squareO
    position <- symbol
    _ <- squareC
    return $ VVArray name position

parseNArray :: ParsecT String u Identity VarValue
parseNArray =  bracketO
           *> liftM NArray (sepBy integer vig)
           <* bracketC



clearS :: String -> String
clearS [] = []
clearS (x:xs) | x == ' ' = clearS xs
              | x == '\n' = clearS xs
              | x == '\t' = clearS xs
              | x == '\r' = clearS xs
              | otherwise = x:clearS xs