module VCGen where

import           Data.Set (Set)
import qualified Data.Set as Se
import           Syntax


vcgen :: Source -> Set LogExp
vcgen (Source pre pos stmts) = Se.union fstC sndC
    where fstC = fstSideCondition pre pos stmts
          sndC = sndSideCondition stmts pos

fstSideCondition :: LogExp -> LogExp -> [Expr]  -> Set LogExp
fstSideCondition pre pos stmts = Se.singleton $ LogBin Imp pre $ wp stmts pos

sndSideCondition :: [Expr] -> LogExp -> Set LogExp
sndSideCondition = vcaux



--Calculation of the vcgen
vcaux :: [Expr] -> LogExp -> Set LogExp
vcaux [] _ = Se.empty
vcaux (x:xs) cond = Se.union (vcaux' x (wp xs cond)) (vcaux xs cond)



vcaux' :: Expr -> LogExp -> Set LogExp
vcaux' (ExprIf _ expr1 expr2) cond =
  Se.union (vcaux expr1 cond) (vcaux expr2 cond)
vcaux' (ExprWhile cond inv expr) cd = Se.union elem1 elem2
  where cond1 = LogBin Imp (LogBin And inv cond) (wp expr inv)
        cond2 = LogBin Imp (LogBin And inv (Not cond)) cd
        elem1 = Se.union (Se.singleton cond1) (Se.singleton cond2)
        elem2 = vcaux  expr inv
vcaux' _ _ = Se.empty


-- Caclculation of the weakest precondition
wp :: [Expr] -> LogExp -> LogExp
wp xs cond = foldr wp' cond xs

wp' :: Expr -> LogExp -> LogExp
wp' (ExprAssign assign) expr = wpAssign assign expr
wp' (ExprIf cond exps1 exps2) expr = wpIf cond exps1 exps2 expr
wp' (ExprWhile _ inv _) _ = inv


-- calculation of weakest precondition If

wpIf :: LogExp -> [Expr] -> [Expr] -> LogExp -> LogExp
wpIf cond exps1 exps2 conExp = LogBin And and1 and2
      where and1 = LogBin Imp cond $  wp exps1 conExp
            and2 = LogBin Imp (Not cond) $ wp exps2 conExp

-- calculation of weakest precondition assign

wpAssign :: Assign -> LogExp -> LogExp
-- no smt criar variavel nova e assert que essa variavel e igual
-- ao valor da expressao
wpAssign (AssignVar nameVar condVar) pos = wpAVLogExp nameVar condVar pos
--Criação de um array declare do array e assert na posicao que é igual a ...
--criação de um array com elementos ? fazer algo com istox
wpAssign (AssignArray _ _ None) posC = posC
wpAssign (AssignArray _ _ (NArray _)) posC = posC -- ???
wpAssign (AssignArray name npos val) posC = wpAALogExp name npos val posC



--Weakest precondition Assignment Array
wpAALogExp :: String -> Int -> ArrayVal -> LogExp -> LogExp
wpAALogExp _ _ _ (BConst bool) = BConst bool
wpAALogExp name pos val (Not expr) = Not (wpAALogExp name pos val expr)

wpAALogExp name pos val (IneBin op exp1 exp2) = IneBin op wpIxp1 wpIxp2
                        where wpIxp1 = wpAAAexp name pos val exp1
                              wpIxp2 = wpAAAexp name pos val exp2
wpAALogExp name pos val (LogBin op exp1 exp2) = LogBin op wpExp1 wpExp2
                        where wpExp1 = wpAALogExp name pos val exp1
                              wpExp2 = wpAALogExp name pos val exp2


wpAAAexp :: String -> Int -> ArrayVal -> AExp -> AExp
wpAAAexp name pos val (SValue  avalue) = SValue $ wpAAAValue name pos val avalue
wpAAAexp name pos val (AExp op exp1 exp2) = AExp op wpExp1 wpExp2
                        where wpExp1 = wpAAAexp name pos val exp1
                              wpExp2 = wpAAAexp name pos val exp2

wpAAAValue :: String -> Int -> ArrayVal -> AValue -> AValue
wpAAAValue name pos val (AArray namePA posPA) | name == namePA && pos == posPA =
                                                    arrayValToAValue val
                                              | otherwise = AArray namePA posPA
wpAAAValue _ _ _ avalue = avalue

arrayValToAValue :: ArrayVal -> AValue
arrayValToAValue (ValArray str n) = AArray str n
arrayValToAValue (ValElem n) = ANum n


--Weakest precondition Assignment Var
wpAVLogExp :: String -> AExp -> LogExp -> LogExp
wpAVLogExp _ _ (BConst bool) = BConst bool
wpAVLogExp nameVar aexp (Not pos) = Not (wpAVLogExp nameVar aexp pos)
wpAVLogExp nameVar aexp (LogBin op exp1 exp2) = LogBin op wpExp1 wpExp2
                        where wpExp1 = wpAVLogExp nameVar aexp exp1
                              wpExp2 = wpAVLogExp nameVar aexp exp2
wpAVLogExp nameVar aexp (IneBin op exp1 exp2) = IneBin op wpIxp1 wpIxp2
                        where wpIxp1 = wpAVAExp nameVar aexp exp1
                              wpIxp2 = wpAVAExp nameVar aexp exp2

wpAVAExp :: String -> AExp -> AExp -> AExp
wpAVAExp nameVar aexp (SValue (AVar name))| nameVar == name = aexp
                                          | otherwise = SValue $ AVar name
wpAVAExp _ _ (SValue val) = SValue val
wpAVAExp nameVar aexp (AExp op exp1 exp2) = AExp op wpExp1 wpExp2
                where wpExp1 = wpAVAExp nameVar aexp exp1
                      wpExp2 = wpAVAExp nameVar aexp exp2




