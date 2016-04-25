module LanguageStructs where

data Program =
    Program [Function] Statement
    deriving Show

data Function =
    Function String [Arg] Statement
    deriving Show

data Arg = 
    EVar String
    | ERef String
    deriving Show

data Var = 
    Var String
    deriving Show

data Ref = 
    Ref String
    deriving Show

data Statement =
    SSeq[Statement]
    | SAssign String AExpr
    | SWhile LExpr Statement
    | SIf LExpr Statement (Maybe Statement)
    | SCall String [AExpr]
    deriving Show

data AExpr =
    Evar String
    | EConst Integer
    | EBinOp ABinOp AExpr AExpr
    deriving Show

data ABinOp =
    Plus
    | Minus
    | Times
    | Div
    deriving Show

data RBinOp =
    Eq
    | NEq
    | L
    | LE
    | G
    | GE
    deriving Show

data LExpr =
    LExpr RBinOp AExpr AExpr
    deriving Show