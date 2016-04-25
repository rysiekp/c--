module Lexer where 
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad

languageDef =
    emptyDef { 
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = ["if", "else", "while", "var", "def", "print", "main"],
        Token.reservedOpNames = ["+", "-", "*", "/", "=", "<", ">", "<=", ">=", "&"]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer