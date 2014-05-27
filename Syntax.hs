module Syntax where
import           Data.List (intercalate)

data Source  = Source LogExp LogExp [Expr]

data Expr = ExprAlloc Alloc
          | ExprAssign Assign
          | ExprIf LogExp [Expr] [Expr] 
          | ExprWhile LogExp LogExp [Expr]

data Alloc = NVar String
           | ANArray  String Int


data Assign = AssignVar String AExp
            | AssignIntArray String Int AExp
            | AssignVarArray String String AExp



data LogExp = BConst Bool
            | Not LogExp
            | Forall String LogExp 
            | Exists String LogExp
            | LogBin LogOp LogExp LogExp
            | IneBin IneOp AExp AExp
                deriving(Ord, Eq)

data LogOp = And | Or | Imp deriving(Ord, Eq)

data IneOp = Lt | Gt | Leq | Geq | Equal | Diff  deriving(Ord, Eq)

data AExp = SAValue VarValue
          | AExp AOp AExp AExp
          deriving(Ord, Eq)

data AOp = Add | Sub | Div | Mul | Mod  deriving(Ord, Eq)

data VarValue = VVNum Int
              | VVar String
              | VIArray String Int
              | VVArray String String
              | NArray [Int]
               deriving(Ord, Eq)

instance Show Source where
  show (Source pre pos exprs) = show pre ++ "\n" ++ fShow exprs ++ show pos
                                where fShow = intercalate "  ".fmap show

instance Show Expr where
  show (ExprAssign assign) = show assign ++ "\n"
  show (ExprAlloc alloc) = show alloc ++ "\n"
  show (ExprIf cond expr1 expr2) = "if( " ++ show cond ++ " ){\n  "
                                 ++ fShow expr1 ++ "}else{\n  "
                                 ++ fShow expr2 ++ "}\n"
                                 where fShow = intercalate "  ".fmap show


  show (ExprWhile cond inv expr) = "while( " ++ show cond ++ "){\n  "
                                    ++ "inv: " ++ show inv  ++";\n  "
                                    ++ fShow expr ++ "}\n"
                                 where fShow = intercalate "  ".fmap show

instance Show Alloc where
    show (NVar var) = "var " ++ var ++ ";" 
    show (ANArray var size) = "var " ++ var ++ "[" ++ show size ++ "]" ++ ";"


instance Show Assign where
    show (AssignVar var expr) = var ++ " = " ++ show expr ++ ";"
    show (AssignIntArray name pos expr) = 
        name ++ "[" ++ show pos ++ "]" ++ " = " ++ show expr ++ show ";"
    show (AssignVarArray name pos expr) = 
        name ++ "[" ++ pos ++ "]" ++ " = " ++ show expr ++ show ";"


instance Show LogExp where
    show (BConst bool) = show bool
    show (Not logexp) = "not " ++ show logexp
    show (Forall var expr) = "forall: " ++ var  ++ ", " ++ show expr
    show (Exists var expr) = "exists: " ++ var  ++ ", " ++ show expr
    show (LogBin op exp1 exp2) ="(" ++ show exp1 ++ show op ++ show exp2 ++ ")"
    show (IneBin op exp1 exp2) ="(" ++ show exp1 ++ show op ++ show exp2 ++ ")"


instance Show AExp where
    show (AExp op exp1 exp2) = "(" ++ show exp1 ++ show op ++ show exp2 ++ ")"
    show (SAValue val) = show val

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

instance Show AOp where
    show Add =  " + "
    show Sub =  " - "
    show Div =  " / "
    show Mul =  " * "
    show Mod = " % "

instance Show VarValue where
    show (VVNum n) = show n
    show (VVar v) = v
    show (VIArray s n) = s ++ "[" ++ show n ++ "]"
    show (VVArray s n) = s ++ "[" ++ n ++"]"
    show (NArray elems) = "{" ++  intercalate ", " (fmap show elems) ++ "}"