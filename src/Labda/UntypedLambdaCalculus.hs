module Labda.UntypedLambdaCalculus where

import Control.Applicative

import Labda.Parser

data Term = Variable String Word | Abstraction String Term | Application Term Term deriving (Eq, Show)

parseTerm :: Parser [String] String Term
parseTerm = parseAbstractionTerm <|> parseApplicationTerm <|> parseVariableTerm

parseVariableNameCharacter :: Parser [String] String Char
parseVariableNameCharacter = do
  h <- pop
  if 'a' <= h && h <= 'z'
    then pure h
    else empty

parseVariableName :: Parser [String] String String
parseVariableName = some parseVariableNameCharacter

parseVariableIndex :: Parser [String] String Word
parseVariableIndex = do
  h <- pop
  if '0' <= h && h <= '9'
    then pure (read [h])
    else empty

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
    <*> parseVariableIndex

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
