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
wp' _ expr = expr




-- calculation of weakest precondition If

wpIf :: LogExp -> [Expr] -> [Expr] -> LogExp -> LogExp
wpIf cond exps1 exps2 conExp = LogBin And and1 and2
      where and1 = LogBin Imp cond $  wp exps1 conExp
            and2 = LogBin Imp (Not cond) $ wp exps2 conExp


-- calculation of weakest precondition assign

wpAssign :: Assign -> LogExp -> LogExp
wpAssign (AssignVar nameVar aexp) = wpAVLogExp nameVar aexp
wpAssign (AssignIntArray nameArr pos aexp) = wpAIA nameArr pos aexp
wpAssign (AssignVarArray nameArr pos aexp) = wpAVA nameArr pos aexp


-- weakest pre condition Assignment Var Array

wpAVA :: String -> String -> AExp -> LogExp -> LogExp
wpAVA _ _ _ (BConst bool) = BConst bool
wpAVA nameVar position aexp  (Forall str pos) = 
    Forall str (wpAVA nameVar position aexp pos)
wpAVA nameVar position aexp (Exists str pos) = 
    Exists str (wpAVA nameVar position aexp pos)
wpAVA nameVar position aexp (Not pos) = 
    Not (wpAVA nameVar position aexp pos)
wpAVA nameVar position aexp (LogBin op exp1 exp2) = LogBin op wpExp1 wpExp2
                        where wpExp1 = wpAVA nameVar position aexp exp1
                              wpExp2 = wpAVA nameVar position aexp exp2
wpAVA nameVar position aexp (IneBin op exp1 exp2) = IneBin op wpIxp1 wpIxp2
                        where wpIxp1 = wpAVVV nameVar position aexp exp1
                              wpIxp2 = wpAVVV nameVar position aexp exp2


wpAVVV :: String -> String -> AExp -> AExp -> AExp
wpAVVV name pos aexp (SAValue (VVArray nameArr posArr )) 
    | name == nameArr && pos == posArr = aexp
    | otherwise = SAValue $ VVArray nameArr posArr
wpAVVV _ _ _ (SAValue val) = SAValue val
wpAVVV name pos aexp (AExp op exp1 exp2) = AExp op wpExp1 wpExp2
                where wpExp1 = wpAVVV name pos aexp exp1
                      wpExp2 = wpAVVV name pos aexp exp2




-- weakest pre condition Assignment Int Array

wpAIA :: String -> Int -> AExp -> LogExp -> LogExp
wpAIA _ _ _ (BConst bool) = BConst bool
wpAIA nameVar position aexp  (Forall str pos) = 
    Forall str (wpAIA nameVar position aexp pos)
wpAIA nameVar position aexp (Exists str pos) = 
    Exists str (wpAIA nameVar position aexp pos)
wpAIA nameVar position aexp (Not pos) = 
    Not (wpAIA nameVar position aexp pos)
wpAIA nameVar position aexp (LogBin op exp1 exp2) = LogBin op wpExp1 wpExp2
                        where wpExp1 = wpAIA nameVar position aexp exp1
                              wpExp2 = wpAIA nameVar position aexp exp2
wpAIA nameVar position aexp (IneBin op exp1 exp2) = IneBin op wpIxp1 wpIxp2
                        where wpIxp1 = wpAIVV nameVar position aexp exp1
                              wpIxp2 = wpAIVV nameVar position aexp exp2


wpAIVV :: String -> Int -> AExp -> AExp -> AExp
wpAIVV name pos aexp (SAValue (VIArray nameArr posArr )) 
    | name == nameArr && pos == posArr = aexp
    | otherwise = SAValue $ VIArray nameArr posArr
wpAIVV _ _ _ (SAValue val) = SAValue val
wpAIVV name pos aexp (AExp op exp1 exp2) = AExp op wpExp1 wpExp2
                where wpExp1 = wpAIVV name pos aexp exp1
                      wpExp2 = wpAIVV name pos aexp exp2

--Weakest precondition Assignment Var
wpAVLogExp :: String -> AExp -> LogExp -> LogExp
wpAVLogExp _ _ (BConst bool) = BConst bool
wpAVLogExp nameVar aexp (Forall str pos) = 
    Forall str (wpAVLogExp nameVar aexp pos)
wpAVLogExp nameVar aexp (Exists str pos) = 
    Exists str (wpAVLogExp nameVar aexp pos)
wpAVLogExp nameVar aexp (Not pos) = Not (wpAVLogExp nameVar aexp pos)
wpAVLogExp nameVar aexp (LogBin op exp1 exp2) = LogBin op wpExp1 wpExp2
                        where wpExp1 = wpAVLogExp nameVar aexp exp1
                              wpExp2 = wpAVLogExp nameVar aexp exp2
wpAVLogExp nameVar aexp (IneBin op exp1 exp2) = IneBin op wpIxp1 wpIxp2
                        where wpIxp1 = wpAVAExp nameVar aexp exp1
                              wpIxp2 = wpAVAExp nameVar aexp exp2

wpAVAExp :: String -> AExp -> AExp -> AExp
wpAVAExp nameVar aexp (SAValue (VVar name))| nameVar == name = aexp
                                           | otherwise = SAValue $ VVar name 
wpAVAExp nameVar aexp (SAValue(VVArray name namePosArr))| nameVar == namePosArr = SAValue $ wpSAVAExp aexp  name
                                                        | otherwise = SAValue $ VVArray name namePosArr

wpAVAExp _ _ (SAValue val) = SAValue val
wpAVAExp nameVar aexp (AExp op exp1 exp2) = AExp op wpExp1 wpExp2
                where wpExp1 = wpAVAExp nameVar aexp exp1
                      wpExp2 = wpAVAExp nameVar aexp exp2


{-
  
 posCond -> a[u] < b[2]

 body -> u = 2 ;

 vc -> a[2] < b[2]
-}

wpSAVAExp :: AExp -> String -> VarValue
wpSAVAExp (SAValue (VVNum n)) str = VIArray str n
wpSAVAExp (SAValue (VVar st)) str = VVArray str st
