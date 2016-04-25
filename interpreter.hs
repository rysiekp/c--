module Main where
import System.Environment
import Parser
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    args <- getArgs
    input <- readFile $ head args
    print $ parse pascalParser (head args) input