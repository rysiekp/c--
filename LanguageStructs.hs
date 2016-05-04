module LanguageStructs where
import Text.PrettyPrint.HughesPJClass

data Program =
    Program [Function] Statement
    deriving Show
instance Pretty Program where 
    pPrint (Program f m) = vcat (map pPrint f) <> pPrint m

data Function =
    Function String [Arg] Statement
    deriving Show
instance Pretty Function where
    pPrint (Function s args stms) = text s <+> parens (hsep (map (<> comma) (map pPrint args))) <+> lbrace <> text "\n" <> (pPrint stms) <> text "\n" <> rbrace <> text "\n"

data Arg = EArg Type Bool String 
    deriving Show
instance Pretty Arg where
    pPrint (EArg t r s) = pPrint t <+> (if r then text "&" else text "") <> pPrint s

data Type
    = TBool
    | TInt
    deriving Show

instance Pretty Type where
    pPrint TBool = text "bool"
    pPrint TInt = text "int"

data Statement 
    = SSeq [Statement]
    | SDeclare Type String Expr
    | SAssign String Expr
    | SWhile Expr Statement
    | SIf Expr Statement (Maybe Statement)
    | SCall String [Expr]
    deriving Show

instance Pretty Statement where
    pPrint (SSeq stms) = vcat (map pPrint stms)
    pPrint (SDeclare t s e) = pPrint t <+> text s <+> text "=" <+> pPrint e <> semi
    pPrint (SAssign s e) = text s <+> text "=" <+> pPrint e <> semi
    pPrint (SWhile l s) = text "while" <+> parens (pPrint l) <+> lbrace <> text "\n" <> (pPrint s) <> text "\n" <> rbrace <> text "\n"
    pPrint (SIf l s Nothing) = text "if" <+> parens (pPrint l) <+> lbrace <> text "\n" <> (pPrint s) <> text "\n" <> rbrace
    pPrint (SIf l s1 (Just s2)) = 
        text "if" <+> parens (pPrint l) <+> lbrace <> text "\n" <> (pPrint s1) <>
        text "\n" <> rbrace <+> text "else" <+> lbrace <> text "\n" <> (pPrint s2) <>
        text "\n" <> rbrace <> text "\n"
    pPrint (SCall s e) = text s <> parens (hsep (map (<> comma) (map pPrint e))) <> semi


data Expr
    = EVar String
    | EIntLit Integer
    | EBoolLit Bool
    | EABinOp ABinOp Expr Expr
    | ELBinOp LBinOp Expr Expr
    | ERBinOp RBinOp Expr Expr
    deriving Show
instance Pretty Expr where
    pPrint (EVar s) = text s
    pPrint (EIntLit i) = integer i
    pPrint (EBoolLit True) = text "true"
    pPrint (EBoolLit _) = text "false"
    pPrint (EABinOp op a b) = parens (pPrint a <+> pPrint op <+> pPrint b)
    pPrint (ELBinOp op a b) = parens (pPrint a <+> pPrint op <+> pPrint b)
    pPrint (ERBinOp op a b) = parens (pPrint a <+> pPrint op <+> pPrint b)

data ABinOp 
    = Plus
    | Minus
    | Times
    | Div
    deriving Show
instance Pretty ABinOp where
    pPrint Plus = text "+"
    pPrint Minus = text "-"
    pPrint Times = text "*"
    pPrint Div = text "/"

data RBinOp 
    = Eq
    | NEq
    | L
    | LE
    | G
    | GE
    deriving Show
instance Pretty RBinOp where
    pPrint Eq = text "=="
    pPrint NEq = text "!="
    pPrint L = text "<"
    pPrint LE = text "<="
    pPrint G = text ">"
    pPrint GE = text ">="

data LBinOp
    = And
    | Or
    deriving Show
instance Pretty LBinOp where
    pPrint And = text "&&"
    pPrint Or = text "||"