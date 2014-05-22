module Syntax where

import           Data.List (intercalate)
data Source = Source LogExp LogExp [Expr] -- pre pos exprs

data Expr = ExprAssign Assign
          | ExprIf LogExp [Expr] [Expr]
          | ExprWhile LogExp LogExp [Expr]

-- Assignements
data Assign = AssignVar String AExp -- b = 2+3 | 2 | 2*(4/[2]) - a
            | AssignArray String Int ArrayVal -- a[2] = ArrayVal

data ArrayVal = ValArray String Int -- c[4]
              | ValElem Int --  Int
              | NArray [Int] -- {1,2,3,4}
              | None -- O None é para quando se aloca um array.

data LogExp = BConst Bool
            | Not LogExp
            | LogBin LogOp LogExp LogExp
            | IneBin IneOp AExp AExp
            deriving(Ord, Eq)

data LogOp = And | Or | Imp deriving (Ord, Eq)

data IneOp = Equal | Diff | Lt | Gt | Leq | Geq deriving (Ord, Eq)

data AExp = AExp AOp AExp AExp
          | SValue AValue
          deriving(Ord, Eq)

data AOp = Add | Sub | Div | Mul deriving(Ord, Eq)

data AValue = ANum Int
            | AVar String
            | AArray String Int
            deriving(Ord, Eq)



instance Show Source where
  show (Source pre pos exprs) = show pre ++ "\n" ++ fShow exprs ++ show pos
                                where fShow = intercalate "  ".fmap show


instance Show Expr where
  show (ExprAssign assign) = show assign ++";\n"
  show (ExprIf cond expr1 expr2) = "if( " ++ show cond ++ " ){\n  "
                                 ++ fShow expr1 ++ "}else{\n  "
                                 ++ fShow expr2 ++ "}\n"
                                 where fShow = intercalate "  ".fmap show


  show (ExprWhile cond inv expr) = "while( " ++ show cond ++ "){\n  "
                                    ++ "inv: " ++ show inv  ++";\n  "
                                    ++ fShow expr ++ "}\n"
                                 where fShow = intercalate "  ".fmap show

instance Show Assign where
  show (AssignVar var expr) = var ++" = " ++ show expr
  show (AssignArray name pos elems) = name ++ "[" ++ show pos ++ "]"
                                      ++ show elems

instance Show ArrayVal where
  show (ValArray name val) = " = " ++ name ++ "[" ++ show val ++ "]"
  show (ValElem arrayElem) =" = " ++ show arrayElem
  show (NArray elems )= " = " ++
                            "{" ++  intercalate ", " (fmap show elems) ++ "}"
  show None = ""



instance Show AExp where
    show (AExp op exp1 exp2) = "( " ++ show exp1 ++ show op ++ show exp2 ++ " )"
    show (SValue val) = show val

instance Show AOp where
    show Add =  " + "
    show Sub =  " - "
    show Div =  " / "
    show Mul =  " * "

instance Show AValue where
    show (ANum n) = show n
    show (AVar str) = str
    show (AArray name val) = name ++ "["++show val++"]"


instance Show LogExp where
    show (BConst bool) = show bool
    show (Not logexp) = "not " ++ show logexp
    show (LogBin op exp1 exp2) =
        "( " ++ show exp1 ++ show op ++ show exp2 ++ " )"
    show (IneBin op exp1 exp2) =
        "( " ++ show exp1 ++ show op ++ show exp2 ++ " )"

instance Show LogOp where
    show And =" && "
    show Or = " || "
    show Imp = " -> "

instance Show IneOp where
    show Equal = " == "
    show Diff = " != "
    show Lt = " < "
    show Gt = " > "
    show Leq = " <= "
    show Geq = ">="
