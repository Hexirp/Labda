module Main ( main ) where

import Control.Applicative

import Labda.Parser
import Labda.UntypedLambdaCalculus

main :: IO ()
main = do
  print $ runParser (symbol "lain" <|> symbol "lambda") "lambda"
  print $ runParser (symbol "JavaScript" <|> symbol "Java") "Java"
