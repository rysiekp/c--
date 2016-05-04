module Main where
import System.Environment
import Parser
import Evaluator
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJClass

instance Pretty ParseError where
    pPrint _ = text "ERROR"

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    print $ parse pascalParser (head args) input
    let parsed = getRight $ parse pascalParser (head args) input
    print $ pPrint parsed
    runEval (evalProgram parsed) ([[]], [])


getRight :: Either a b -> b
getRight (Right x) = x