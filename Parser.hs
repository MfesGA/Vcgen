module Parser where

import           AuxParsers                               as Aux
import           Control.Applicative                      hiding (many, (<|>))
import           Control.Monad
import           Data.Functor.Identity
import           Syntax
import           Text.Parsec.Expr                         as Pex
import           Text.Parsec.Prim                         as Prim
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Combinator
import           Text.ParserCombinators.Parsec.Prim


parseFile :: String -> IO (Either String Source)
parseFile str = do
    let result = Prim.parse parseSource "error.hs" $ clearS str
    case result of
      Left err -> return $ Left $ show err
      Right val -> return $ Right $ val

-- Parse Source

parseSource :: ParsecT String u Identity Source
parseSource = do
    pre <- parsePre
    exprs <- many $ Prim.try parseExpr
    pos <- parsePos
    return $ Source pre pos exprs



-- Parse Assignments

parseInv :: ParsecT String u Identity LogExp
parseInv = inv *> parseLogExp <* sep

parsePre :: ParsecT String u Identity LogExp
parsePre = pre *> parseLogExp <* sep

parsePos :: ParsecT String u Identity LogExp
parsePos = pos *> parseLogExp <* sep


parseExpr :: ParsecT String u Identity Expr
parseExpr = parseIf <|> parseWhile <|> parseExprAssign

parseWhile :: ParsecT String u Identity Expr
parseWhile = do
    while
    parO
    cond <-  parseLogExp
    parC
    bracketO
    inv <- parseInv
    exprs <- many parseExpr
    bracketC
    return $ ExprWhile cond inv exprs



parseIf :: ParsecT String u Identity Expr
parseIf = do
    pIf
    parO
    cond <- parseLogExp
    parC
    bracketO
    exprs1 <- many parseExpr
    bracketC
    pElse
    bracketO
    exprs2 <- many parseExpr
    bracketC
    return $ ExprIf cond exprs1 exprs2

parseExprAssign :: ParsecT String u Identity Expr
parseExprAssign = liftM ExprAssign parseAssign <* sep

parseAssign :: ParsecT String u Identity Assign
parseAssign = do
    name <- symbol
    maybeArray <- parseMaybeArray
    case maybeArray of
        Nothing -> parseAssignVar name
        (Just pos) -> parseAssignArray name pos

parseMaybeArray :: ParsecT String u Identity (Maybe Int)
parseMaybeArray = optionMaybe $ squareO *> integer <* squareC

parseAssignVar :: String ->  ParsecT String u Identity Assign
parseAssignVar name = do
    eq
    aExp <- parseAExp
    return $ AssignVar name aExp



parseAssignArray :: String -> Int -> ParsecT String u Identity Assign
parseAssignArray name pos = do
    mEq <- optionMaybe eq
    case mEq of
        Nothing -> return $ AssignArray name pos None -- Criação de array
        Just _ -> liftM  (AssignArray name pos) parseArrayVal -- Valor do array


parseArrayVal :: ParsecT String u Identity ArrayVal
parseArrayVal  = parseVarElem  <|> parseValArray <|> parseNArray


parseNArray :: ParsecT String u Identity ArrayVal
parseNArray = bracketO
           *> liftM NArray (sepBy integer vig)
           <* bracketC




parseValArray :: ParsecT String u Identity ArrayVal
parseValArray = do
    name <- symbol
    squareO
    pos <- integer
    squareC
    return $  ValArray name pos

parseVarElem :: ParsecT String u Identity ArrayVal
parseVarElem = liftM ValElem integer






-- Parse LogExp

parseLogExp :: ParsecT String u Identity LogExp
parseLogExp = buildExpressionParser logExpTable logExpFactor



logExpTable :: [[Operator String u Identity LogExp]]
logExpTable  = [ [ Prefix (pNot >> return Not) ]
               , [ Infix (liftM LogBin parseImp) AssocLeft]
               , [ Infix (liftM LogBin parseAnd) AssocLeft]
               , [ Infix (liftM LogBin parseOr) AssocLeft]
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
parseIneOp = Prim.try parseEqual
       <|> Prim.try parseLeq
       <|> Prim.try parseGeq
       <|> Prim.try parseDiff
       <|> Prim.try parseLt
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

-- Parse Boolean

parseBConst :: ParsecT String u Identity LogExp
parseBConst = liftM BConst parseBool

parseBool :: ParsecT String u Identity Bool
parseBool = parseTrue <|> parseFalse

parseTrue :: ParsecT String u Identity Bool
parseTrue =  true *> return True

parseFalse :: ParsecT String u Identity Bool
parseFalse = false *> return False


-- ParseAExp
parseAExp :: ParsecT String u Identity AExp
parseAExp = buildExpressionParser aExpTable aExpFactor

aExpTable :: [[Operator String u Identity AExp]]
aExpTable = [ [ Infix (liftM AExp parseMul ) AssocLeft
              , Infix (liftM AExp parseDiv) AssocLeft]
            , [ Infix (liftM AExp parseSum) AssocLeft
              , Infix (liftM AExp parseSub) AssocLeft]
            ]
aExpFactor :: ParsecT String u Identity AExp
aExpFactor = parseExpPar <|> parseSVal

parseExpPar :: ParsecT String u Identity AExp
parseExpPar = parO *> parseAExp <* parC

parseSVal :: ParsecT String u Identity AExp
parseSVal = liftM SValue parseAVal

parseSum :: ParsecT String u Identity AOp
parseSum = Aux.sum *> return Add

parseSub :: ParsecT String u Identity AOp
parseSub = sub *> return Sub

parseMul :: ParsecT String u Identity AOp
parseMul = mul *> return Mul

parseDiv :: ParsecT String u Identity AOp
parseDiv = Aux.div *> return Div


parseAVal :: ParsecT String u Identity AValue
parseAVal = parseANum <|> parseAVarArr

parseANum :: ParsecT String u Identity AValue
parseANum = liftM ANum integer

parseAVarArr :: ParsecT String u Identity AValue
parseAVarArr = do
    symb <- symbol
    arr <- optionMaybe parseMArr
    case arr of
        Nothing -> return $ AVar symb
        Just val -> return $ AArray symb val


parseMArr :: ParsecT String u Identity Int
parseMArr = squareO *> integer <* squareC


clearS :: String -> String
clearS [] = []
clearS (x:xs) | x == ' ' = clearS xs
              | x == '\n' = clearS xs
              | otherwise = x:clearS xs
