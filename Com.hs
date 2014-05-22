module Com where

import           Data.Set          (Set)
import qualified Data.Set          as Se
import           Hsmtlib.HighLevel
import           SMTLib2
import           SMTLib2.Array
import           SMTLib2.Core      as C
import           SMTLib2.Int
import           Syntax            hiding (Expr)


getAsserts :: Set LogExp -> Set Expr
getAsserts = Se.map createSexpr

getVars :: Set LogExp -> Set String
getVars = Se.foldr fun Se.empty
        where fun =Se.union.getVarSexpr

-- Creates the expressions for the assertions

createSexpr :: LogExp -> Expr
createSexpr (BConst b) = boolToExpr b
createSexpr (Not a) =  C.not (createSexpr a)
createSexpr (LogBin logop expr1 expr2) = logBinToExpr logop expr1 expr2
createSexpr (IneBin ineop axp1 axp2) = ineBinToExpr ineop axp1 axp2


logBinToExpr :: LogOp -> LogExp -> LogExp -> Expr
logBinToExpr And expr1 expr2 = C.and (createSexpr expr1) (createSexpr expr2)
logBinToExpr Or expr1 expr2 = C.or (createSexpr expr1) (createSexpr expr2)
logBinToExpr Imp expr1 expr2 = createSexpr expr1 ==> createSexpr expr2

boolToExpr :: Bool -> Expr
boolToExpr True = true
boolToExpr False = false

ineBinToExpr :: IneOp -> AExp -> AExp -> Expr
ineBinToExpr Equal aexp1 aexp2 =  aExpToExpr aexp1 ===  aExpToExpr aexp2
ineBinToExpr Diff aexp1 aexp2 = aExpToExpr aexp1 =/=  aExpToExpr aexp2
ineBinToExpr Lt aexp1 aexp2 = nLt (aExpToExpr aexp1) (aExpToExpr aexp2)
ineBinToExpr Gt aexp1 aexp2 = nGt (aExpToExpr aexp1) (aExpToExpr aexp2)
ineBinToExpr Leq aexp1 aexp2 = nLeq (aExpToExpr aexp1) (aExpToExpr aexp2)
ineBinToExpr Geq aexp1 aexp2 = nGeq (aExpToExpr aexp1) (aExpToExpr aexp2)


aExpToExpr :: AExp -> Expr
aExpToExpr (AExp aop aexp1 aexp2) = aOpToExpr aop aexp1 aexp2
aExpToExpr (SValue avalue) = aValueToExpr avalue


aOpToExpr :: AOp -> AExp -> AExp -> Expr
aOpToExpr Add  aexp1 aexp2 = nAdd (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Sub aexp1 aexp2 = nSub (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Div aexp1 aexp2 = nDiv (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Mul aexp1 aexp2 = nMul (aExpToExpr aexp1) (aExpToExpr aexp2)



aValueToExpr :: AValue -> Expr
aValueToExpr (ANum n) = literal n
aValueToExpr (AVar v) = constant v
aValueToExpr (AArray name pos) = select (constant name) (literal pos)



-- Gets a Set of variables

getVarSexpr :: LogExp -> Set String
getVarSexpr (BConst _) = Se.empty
getVarSexpr (Not a) =  getVarSexpr a
getVarSexpr (LogBin _ expr1 expr2) =
    Se.union (getVarSexpr expr1)  (getVarSexpr expr2)
getVarSexpr (IneBin _ axp1 axp2) =
    Se.union (getVarAexp axp1) (getVarAexp axp2)


getVarAexp :: AExp -> Set String
getVarAexp (AExp _ aexp1 aexp2) =
    Se.union (getVarAexp aexp1) (getVarAexp aexp2)
getVarAexp (SValue avalue) =
    getVarAvalue avalue



getVarAvalue :: AValue -> Set String
getVarAvalue (AVar v) = Se.singleton v
getVarAvalue _ = Se.empty
