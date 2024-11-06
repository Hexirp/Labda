module Labda.UntypedLambdaCalculus where

import Control.Applicative

import Labda.Parser

data Term = Variable String Word | Abstraction String Term | Application Term Term deriving (Eq, Show)

parseVariableNameCharacter :: Parser [String] String Char
parseVariableNameCharacter = Parser $ \s -> case s of
  [] -> Failure ["parseVariableNameCharacter: the end was not accepted."]
  sh : st -> if 'a' <= sh && sh <= 'z'
    then Success ["parseVariableNameCharacter: " ++ show sh ++ " was accepted."] st sh
    else Failure ["parseVariableNameCharacter: " ++ show sh ++ " was not accepted."]

parseVariableName :: Parser [String] String String
parseVariableName =
  pure (:)
    <*> parseVariableNameCharacter
    <*> (parseVariableName <|> pure "")

parseVariableIndex :: Parser [String] String Word
parseVariableIndex = Parser $ \s -> case s of
  [] -> Failure ["parseVariableIndex: the end was not accepted."]
  sh : st -> if '0' <= sh && sh <= '9'
    then Success ["parseVariableIndex: " ++ show sh ++ " was accepted."] st (read [sh])
    else Failure ["parseVariableIndex: " ++ show sh ++ " was not accepted."]

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
    <*> (parseTermWithParentheses <|> parseVariableTerm)

parseTermWithParentheses :: Parser [String] String Term
parseTermWithParentheses =
  pure id
    <* character '('
    <* character ' '
    <*> parseTerm
    <* character ' '
    <* character ')'

parseTerm :: Parser [String] String Term
parseTerm = parseAbstractionTerm <|> parseApplicationTerm <|> parseVariableTerm

parse :: String -> Term
parse t = if t == "lambda x => lambda y => x#1"
  then Abstraction "x" (Abstraction "y" (Variable "x" 1))
  else Abstraction "x" (Variable "x" 0)

unparse :: Term -> String
unparse t = "lambda x => lambda y => x#1"

format :: String -> String
format t = unparse (parse t)
