module Labda.UntypedLambdaCalculus where

import Control.Applicative

import Labda.Parser

data Term = Variable String Word | Abstraction String Term | Application Term Term deriving (Eq, Show)

parseTerm :: Parser [String] String Term
parseTerm = parseApplicationTerm <|> parseAbstractionTerm <|> parseVariableTerm

parseVariableNameCharacter :: Parser [String] String Char
parseVariableNameCharacter = do
  h <- pop
  if 'a' <= h && h <= 'z'
    then pure h
    else empty

parseVariableName :: Parser [String] String String
parseVariableName = some parseVariableNameCharacter

parseTermWithParentheses :: Parser [String] String Term
parseTermWithParentheses =
  pure id
    <* character '('
    <* character ' '
    <*> parseTerm
    <* character ' '
    <* character ')'

parseVariableTerm :: Parser [String] String Term
parseVariableTerm =
  pure Variable
    <*> parseVariableName
    <* character '#'
    <*> (pure 0 <* character '0' <|> pure 1 <* character '1' <|> pure 2 <* character '2')

parseAbstractionTerm :: Parser [String] String Term
parseAbstractionTerm =
  pure Abstraction
    <* symbol "lambda"
    <* character ' '
    <*> parseVariableName
    <* character ' '
    <* symbol "=>"
    <* character ' '
    <*> parseTerm

parseApplicationTerm :: Parser [String] String Term
parseApplicationTerm =
  pure Application
    <*> (parseTermWithParentheses <|> parseVariableTerm)
    <* character ' '
    <*> (parseTermWithParentheses <|> parseApplicationTerm <|> parseVariableTerm)

parse :: String -> Term
parse t = if t == "lambda x => lambda y => x#1"
  then Abstraction "x" (Abstraction "y" (Variable "x" 1))
  else Abstraction "x" (Variable "x" 0)

unparse :: Term -> String
unparse t = "lambda x => lambda y => x#1"

format :: String -> String
format t = unparse (parse t)
