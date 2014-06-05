module Com where

import           Data.Set          (Set)
import qualified Data.Set          as Se
import           Hsmtlib.HighLevel
import           SMTLib2           as SL
import           SMTLib2.Array
import           SMTLib2.Core      as C
import           SMTLib2.Int
import           Syntax            as S



getAsserts :: Set LogExp -> Set SL.Expr
getAsserts = Se.map createSexpr

getVars :: Set LogExp -> Set String
getVars = Se.foldr foo Se.empty
        where foo = Se.union.getVarSexpr

getExpArrays :: Set LogExp -> Set String
getExpArrays = Se.foldr foo Se.empty
        where foo = Se.union.getArraySexpr

getArrays :: Source -> Set String
getArrays (Source _ _ stmts) = getArrayLExpr stmts


-- Creates the expressions for the assertions

createSexpr :: LogExp -> SL.Expr
createSexpr (BConst b) = boolToExpr b
createSexpr (Not a) =  C.not (createSexpr a)
createSexpr (S.Forall s expr) = forall [bind s tInt] (createSexpr expr)
createSexpr (S.Exists s expr) = exists [bind s tInt] (createSexpr expr)
createSexpr (LogBin logop expr1 expr2) = logBinToExpr logop expr1 expr2
createSexpr (IneBin ineop axp1 axp2) = ineBinToExpr ineop axp1 axp2


logBinToExpr :: LogOp -> LogExp -> LogExp -> SL.Expr
logBinToExpr And expr1 expr2 = C.and (createSexpr expr1) (createSexpr expr2)
logBinToExpr Or expr1 expr2 = C.or (createSexpr expr1) (createSexpr expr2)
logBinToExpr Imp expr1 expr2 = createSexpr expr1 ==> createSexpr expr2

boolToExpr :: Bool -> SL.Expr
boolToExpr True = true
boolToExpr False = false

ineBinToExpr :: IneOp -> AExp -> AExp -> SL.Expr
ineBinToExpr Equal aexp1 aexp2 =  aExpToExpr aexp1 ===  aExpToExpr aexp2
ineBinToExpr Diff aexp1 aexp2 = aExpToExpr aexp1 =/=  aExpToExpr aexp2
ineBinToExpr Lt aexp1 aexp2 = nLt (aExpToExpr aexp1) (aExpToExpr aexp2)
ineBinToExpr Gt aexp1 aexp2 = nGt (aExpToExpr aexp1) (aExpToExpr aexp2)
ineBinToExpr Leq aexp1 aexp2 = nLeq (aExpToExpr aexp1) (aExpToExpr aexp2)
ineBinToExpr Geq aexp1 aexp2 = nGeq (aExpToExpr aexp1) (aExpToExpr aexp2)


aExpToExpr :: AExp -> SL.Expr
aExpToExpr (AExp aop aexp1 aexp2) = aOpToExpr aop aexp1 aexp2
aExpToExpr (SAValue avalue) = aValueToExpr avalue


aOpToExpr :: AOp -> AExp -> AExp -> SL.Expr
aOpToExpr Add aexp1 aexp2 = nAdd (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Sub aexp1 aexp2 = nSub (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Div aexp1 aexp2 = nDiv (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Mul aexp1 aexp2 = nMul (aExpToExpr aexp1) (aExpToExpr aexp2)
aOpToExpr Mod aexp1 aexp2 = nMod (aExpToExpr aexp1) (aExpToExpr aexp2)


aValueToExpr :: VarValue -> SL.Expr
aValueToExpr (VVNum n) = lit n
aValueToExpr (VVar v) = ct v
aValueToExpr (VIArray name pos) = select (ct name) (lit pos)
aValueToExpr (VVArray name pos) = select (ct name) (ct pos)


-- Gets a Set of variables

getVarSexpr :: LogExp -> Set String
getVarSexpr (BConst _) = Se.empty
getVarSexpr (Not a) =  getVarSexpr a
getVarSexpr (S.Forall _ expr) = getVarSexpr expr
getVarSexpr (S.Exists _ expr) = getVarSexpr expr
getVarSexpr (LogBin _ expr1 expr2) =
    Se.union (getVarSexpr expr1)  (getVarSexpr expr2)
getVarSexpr (IneBin _ axp1 axp2) =
    Se.union (getVarAexp axp1) (getVarAexp axp2)


getVarAexp :: AExp -> Set String
getVarAexp (AExp _ aexp1 aexp2) =
    Se.union (getVarAexp aexp1) (getVarAexp aexp2)
getVarAexp (SAValue avalue) =
    getVarAvalue avalue



getVarAvalue :: VarValue-> Set String
getVarAvalue (VVar v) = Se.singleton v
getVarAvalue (VIArray name pos) = Se.singleton (name ++ show pos)
getVarAvalue (VVArray name pos) = Se.singleton (name ++ pos)
getVarAvalue _ = Se.empty




-- Get the arrays used in the vcs

getArraySexpr :: LogExp -> Set String
getArraySexpr (BConst _) = Se.empty
getArraySexpr (Not a) =  getArraySexpr a
getArraySexpr (S.Forall _ expr) = getArraySexpr expr
getArraySexpr (S.Exists _ expr) = getArraySexpr expr
getArraySexpr (LogBin _ expr1 expr2) =
    Se.union (getArraySexpr expr1)  (getArraySexpr expr2)
getArraySexpr (IneBin _ axp1 axp2) =
    Se.union (getArrayAexp axp1) (getArrayAexp axp2)


getArrayAexp :: AExp -> Set String
getArrayAexp (AExp _ aexp1 aexp2) =
    Se.union (getArrayAexp aexp1) (getArrayAexp aexp2)
getArrayAexp (SAValue avalue) =
    getArrayAvalue avalue



getArrayAvalue :: VarValue-> Set String
getArrayAvalue (VIArray name _) = Se.singleton name
getArrayAvalue (VVArray name _) = Se.singleton name
getArrayAvalue _ = Se.empty





--  Gets the  allocated arrays

getArrayLExpr :: [S.Expr] -> Set String
getArrayLExpr = foldr (Se.union . getArrayExpr) Se.empty

getArrayExpr :: S.Expr -> Set String
getArrayExpr (ExprIf _ expr1 expr2) =
    Se.union (getArrayLExpr expr1) (getArrayLExpr expr2)
getArrayExpr (ExprWhile _ _ expr) = getArrayLExpr expr
getArrayExpr (ExprAlloc alloc ) = getArrayAlloc alloc
getArrayExpr _ = Se.empty


getArrayAlloc :: Alloc -> Set String
getArrayAlloc (ANArray name _) = Se.singleton name
getArrayAlloc _ = Se.empty

