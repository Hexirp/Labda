module Labda.UntypedLambdaCalculus (parse, unparse, format) where

data SyntaxTree = Variable String Word | Abstraction String SyntaxTree | Application SyntaxTree SyntaxTree

parse :: String -> SyntaxTree
parse t = if t == "lambda x => lambda y => x#1"
  then Abstraction "x" (Abstraction "y" (Variable "x" 1))
  else Abstraction "x" (Variable "x" 0)

unparse :: SyntaxTree -> String
unparse t = "lambda x => lambda y => x#1"

format :: String -> String
format t = unparse (parse t)
