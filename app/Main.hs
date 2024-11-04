module Main ( main ) where

import Control.Applicative

import Labda.Parser
import Labda.UntypedLambdaCalculus

printParserResult :: Show a => ParserResult [String] String a -> IO ()
printParserResult r = case r of
  Failure w -> do
    putStrLn "= Failure ="
    putStrLn ""
    putStrLn $ unlines w
  Success w s a -> do
    putStrLn "= Success ="
    putStrLn ""
    putStrLn $ unlines w
    print s
    putStrLn ""
    print a
    putStrLn ""

main :: IO ()
main = do
  printParserResult $ runParser (symbol "lain" <|> symbol "lambda") "lambda"
  printParserResult $ runParser (symbol "JavaScript" <|> symbol "Java") "Java"
  printParserResult $ runParser parseTerm "lambda x => lambda y => x#1"
  printParserResult $ runParser parseTerm "lambda x => ( lambda y => x#1 ( y#0 y#0 ) ) ( lambda y => x#1 ( y#0 y#0 ) )"
