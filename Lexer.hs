module Lexer where 
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec
import Data.Functor.Identity
import Text.Parsec.Prim

languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
    emptyDef { 
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = ["if", "else", "while", "def", 
                               "main", "true", "false", "int", "bool"],
        Token.reservedOpNames = ["+", "-", "*", "/",
                                 "=", "<", ">", "<=", ">=", "==",
                                 "&&", "||",
                                 "++", "+=", "--", "-=", "*=", "/="]
    }

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDef

identifier :: Text.Parsec.Prim.ParsecT String u Identity String
identifier = Token.identifier lexer
reserved :: String -> Text.Parsec.Prim.ParsecT String u Identity ()
reserved = Token.reserved lexer
reservedOp :: String -> Text.Parsec.Prim.ParsecT String u Identity ()
reservedOp = Token.reservedOp lexer
parens :: Text.Parsec.Prim.ParsecT String u Identity a -> Text.Parsec.Prim.ParsecT String u Identity a
parens = Token.parens lexer
braces :: Text.Parsec.Prim.ParsecT String u Identity a -> Text.Parsec.Prim.ParsecT String u Identity a
braces = Token.braces lexer
integer :: Text.Parsec.Prim.ParsecT String u Identity Integer
integer = Token.integer lexer
semi :: Text.Parsec.Prim.ParsecT String u Identity String
semi = Token.semi lexer
whiteSpace :: Text.Parsec.Prim.ParsecT String u Identity ()
whiteSpace = Token.whiteSpace lexer
comma :: Text.Parsec.Prim.ParsecT String u Identity String
comma = Token.comma lexer
colon :: Text.Parsec.Prim.ParsecT String u Identity String
colon = Token.colon lexer