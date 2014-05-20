module Main where

import           Com
import           Control.Applicative ((<**>),pure)
import           Data.Set            (Set)
import qualified Data.Set            as Se
import           Hsmtlib
import           Hsmtlib.HighLevel   as H
import           Hsmtlib.Solver      as Sl
import           Parser
import           SMTLib2
import           SMTLib2.Int
import           Syntax              hiding (Expr)
import           System.IO
import           VCGen

flip3 ::  (a -> b -> c -> IO d) -> b -> c -> a -> IO d
flip3 f b c a = f a b c

declareCt :: String -> (Solver ->  IO GenResult)
declareCt name = flip3 (H.declConst) name tInt


declareVars :: Set LogExp -> [Solver -> IO GenResult]
declareVars  exprs = fmap (declareCt) (Se.toList.getVars $ exprs)
 

check :: Set LogExp -> IO SatResult
check exprs = do
    solver <- startSolver Z3 Online "AUFLIA" Nothing Nothing
    produceModels solver
    mapDeclConst solver (Se.toList.getVars $ exprs) tInt
    maping solver (createSexpr) (Se.toList exprs)
    val <- checkSat solver
    getValue solver [constant "x"] >>= print
    exit solver
    return val




main = do
    fHandle <- openFile "files/teste5.c" ReadMode
    file <- hGetContents fHandle
    res <- parseFile file
    case res of
        Left err -> print err >> hClose fHandle
        Right source -> check (vcgen source) >>= print >> hClose fHandle
    