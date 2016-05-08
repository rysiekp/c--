module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import LanguageStructs
import Lexer
import System.Environment
import Control.Monad
import Control.Applicative ((<*))

pascalParser :: Parser Program
pascalParser = whiteSpace >> program

program :: Parser Program
program =
    do functions <- (many (try function))
       mainF <- main' <* eof
       return $ Program functions mainF

main' :: Parser Statement
main' =
    do  reserved "main"
        statements <- block
        return statements

function :: Parser Function
function =
    do reserved "def"
       name <- identifier
       args <- argumentList
       statements <- block
       return $ Function  name args statements


argumentList :: Parser [Arg]
argumentList = parens (sepBy argument comma)

argument :: Parser Arg
argument = 
    do  t <- typeP
        ref <- refP
        id <- identifier
        return $ EArg t ref id

typeP :: Parser Type
typeP = 
    (reserved "int" >> return TInt) <|> (reserved "bool" >> return TBool)

refP :: Parser Bool
refP = try (reservedOp "&" >> return True) <|> return False

block :: Parser Statement
block = braces sequenceOfStatement
 
sequenceOfStatement :: Parser Statement
sequenceOfStatement =
  do list <- (many1 statement)
     return $ if length list == 1 then head list else SSeq list

statement :: Parser Statement
statement = ifStatement <|> 
            whileStatement <|> 
            try declareStatement <|>
            try opAssignStatement <|>
            try assignStatement <|>
            callStatement

opAssignStatement :: Parser Statement
opAssignStatement =
    do var <- identifier
       op <- opAssignment
       semi
       return $ SOpAss var op

opAssignment :: Parser AOp
opAssignment = 
    try (reservedOp "++" >> return PlusPlus) <|>
    try (reservedOp "--" >> return MinusMinus) <|>
    assOp
     

assOp :: Parser AOp
assOp = do
    op <- aOp
    e <- expression
    return $ OpAss op e

aOp :: Parser ABinOp
aOp =
    (reserved "+=" >> return Plus) <|>
    (reserved "-=" >> return Minus) <|>
    (reserved "*=" >> return Times) <|>
    (reserved "/=" >> return Div)

ifStatement :: Parser Statement
ifStatement =
  do reserved "if"
     cond <- expression
     statement1 <- block
     try (elseStatement cond statement1) <|> noElse cond statement1

elseStatement :: Expr -> Statement -> Parser Statement
elseStatement cond statement1 =
    do reserved "else"
       statement2 <- block
       return $ SIf cond statement1 (Just statement2)

noElse :: Expr -> Statement -> Parser Statement
noElse cond statement1 = return $ SIf cond statement1 Nothing

 
whileStatement :: Parser Statement
whileStatement =
  do reserved "while"
     cond <- expression
     statement' <- block
     return $ SWhile cond statement'

assignStatement :: Parser Statement
assignStatement =
    do var <- identifier
       reservedOp "="
       expr <- expression
       semi
       return $ SAssign var expr
 
declareStatement :: Parser Statement
declareStatement =
  do t <- decType
     var <- identifier
     reservedOp "="
     expr <- expression
     semi
     return $ SDeclare t var expr

decType :: Parser Type
decType = (reserved "int" >> return TInt) <|> (reserved "bool" >> return TBool)

callStatement :: Parser Statement
callStatement =
    do id <- identifier
       params <- parameterList
       semi
       return $ SCall id params

parameterList :: Parser [Expr]
parameterList = parens (sepBy expression comma)

expression :: Parser Expr
expression = try aExpression <|> lExpression

aExpression :: Parser Expr
aExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Infix (reservedOp "*" >> return (EABinOp Times)) AssocLeft,
                Infix (reservedOp "/" >> return (EABinOp Div)) AssocLeft],
               [Infix (reservedOp "+" >> return (EABinOp Plus)) AssocLeft,
                Infix (reservedOp "-" >> return (EABinOp Minus)) AssocLeft]]

aTerm =  parens aExpression <|> liftM EIntLit integer <|> liftM EVar identifier

lExpression :: Parser Expr
lExpression = buildExpressionParser lOperators lTerm

lTerm = try (parens lExpression) <|>
        (reserved "true" >> return (EBoolLit True)) <|>
        (reserved "false" >> return (EBoolLit False)) <|>
        liftM EVar identifier <|>
        parens rExpression

lOperators = [ [Infix (reservedOp "&&" >> return (ELBinOp And)) AssocLeft,
                Infix (reservedOp "||" >> return (ELBinOp Or)) AssocLeft]]

rExpression :: Parser Expr
rExpression = buildExpressionParser rOperators expression

rOperators = [ [Infix (reservedOp "<" >> return (ERBinOp L)) AssocNone,
                Infix (reservedOp "<=" >> return (ERBinOp LE)) AssocNone,
                Infix (reservedOp ">" >> return (ERBinOp G)) AssocNone,
                Infix (reservedOp ">=" >> return (ERBinOp GE)) AssocNone,
                Infix (reservedOp "==" >> return (ERBinOp Eq)) AssocNone,
                Infix (reservedOp "!=" >> return (ERBinOp NEq)) AssocNone]]