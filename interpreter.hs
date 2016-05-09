module Main where
import System.Environment
import Parser
import Evaluator
import TypeChecker
import Text.ParserCombinators.Parsec
import Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    case parse pascalParser (head args) input of
        Left err ->  print err
        Right parsed -> do
            check <- runCheck (checkProgram parsed) (Map.empty, Map.empty)
            getRight check
            eval <- runEval (evalProgram parsed) (Map.empty, Map.empty)
            getRight eval

getRight :: Either String b -> IO b
getRight (Right x) = return x
getRight (Left err) = error err