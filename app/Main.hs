module Main ( main ) where

import Control.Applicative

import Labda.Parser
import Labda.UntypedLambdaCalculus

main :: IO ()
main = print $ runParser (symbol "lain" <|> symbol "lambda") "lambda"
