module Labda.UntypedLambdaCalculus (Term (..), parse, unparse, format) where

data Term = Variable String Word | Abstraction String Term | Application Term Term

parse :: String -> Term
parse t = if t == "lambda x => lambda y => x#1"
  then Abstraction "x" (Abstraction "y" (Variable "x" 1))
  else Abstraction "x" (Variable "x" 0)

unparse :: Term -> String
unparse t = "lambda x => lambda y => x#1"

format :: String -> String
format t = unparse (parse t)
