module Parser where
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import LanguageStructs
import Lexer
import System.Environment
import Control.Monad

pascalParser :: Parser Program
pascalParser = whiteSpace >> program

program :: Parser Program
program =
    do functions <- (many (try function))
       mainF <- main'
       return $ Program functions mainF

main' :: Parser Statement
main' =  
    do reserved "def"
       reserved "main"
       argumentList
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
argumentList = 
    parens (sepBy argument comma) >>= \list -> return $ list

argument :: Parser Arg
argument = reference <|> value

reference :: Parser Arg
reference = 
    do reservedOp "&"
       id <- identifier
       return $ ERef id

value :: Parser Arg
value = 
    do
        id <- identifier
        return $ EVar id

block :: Parser Statement
block = braces sequenceOfStatement
 
sequenceOfStatement :: Parser Statement
sequenceOfStatement =
  do list <- (many1 statement)
     return $ if length list == 1 then head list else SSeq list

statement :: Parser Statement
statement = ifStatement <|> 
            whileStatement <|> 
            assignStatement <|>
            callStatement

ifStatement :: Parser Statement
ifStatement =
  do reserved "if"
     cond <- lExpression
     statement1 <- block
     try (elseStatement cond statement1) <|> noElse cond statement1

elseStatement :: LExpr -> Statement -> Parser Statement
elseStatement cond statement1 =
    do reserved "else"
       statement2 <- block
       return $ SIf cond statement1 (Just statement2)

noElse :: LExpr -> Statement -> Parser Statement
noElse cond statement1 = return $ SIf cond statement1 Nothing

 
whileStatement :: Parser Statement
whileStatement =
  do reserved "while"
     cond <- lExpression
     statement'' <- block
     return $ SWhile cond statement''
 
assignStatement :: Parser Statement
assignStatement =
  do reserved "var"
     var <- identifier
     reservedOp "="
     expr <- aExpression
     semi
     return $ SAssign var expr

callStatement :: Parser Statement
callStatement =
    do id <- identifier
       params <- parameterList
       semi
       return $ SCall id params

parameterList :: Parser [AExpr]
parameterList =
    parens (sepBy aExpression comma) >>= \list -> return $ list


aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

aOperators = [ [Infix (reservedOp "*" >> return (EBinOp Times)) AssocLeft,
                Infix (reservedOp "/" >> return (EBinOp Div)) AssocLeft],
               [Infix (reservedOp "+" >> return (EBinOp Plus)) AssocLeft,
                Infix (reservedOp "-" >> return (EBinOp Minus)) AssocLeft]]

aTerm =  parens aExpression <|> liftM Evar identifier <|> liftM EConst integer

lExpression :: Parser LExpr
lExpression = buildExpressionParser [] bTerm

bTerm =  parens lExpression <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ LExpr op a1 a2
 
relation = (reservedOp ">" >> return G) <|> 
           (reservedOp "<" >> return L) <|>
           (reservedOp "<=" >> return LE) <|>
           (reservedOp ">=" >> return GE) <|>
           (reservedOp "==" >> return Eq) <|>
           (reservedOp "!=" >> return NEq)