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
    <*> (parseAbstractionTerm <|> parseApplicationTerm)
    <* character ' '
    <* character ')'

parseTerm :: Parser [String] String Term
parseTerm = parseAbstractionTerm <|> parseApplicationTerm <|> parseVariableTerm

parse :: String -> Maybe Term
parse t = fromParserResultToMaybe (runParser parseTerm t)

unparseVariableTerm :: String -> Word -> String
unparseVariableTerm s i = s ++ "#" ++ show i

unparseAbstractionTerm :: String -> Term -> String
unparseAbstractionTerm s t = "lambda " ++ s ++ " => " ++ unparseTerm t

unparseApplicationTerm :: Term -> Term -> String
unparseApplicationTerm t0 t1 = unparseTermWithParentheses t0 ++ " " ++ unparseTermWithParentheses t1

unparseTermWithParentheses :: Term -> String
unparseTermWithParentheses t = case t of
  Variable s i -> unparseVariableTerm s i
  Abstraction s t0 -> "( " ++ unparseAbstractionTerm s t0 ++ " )"
  Application t0 t1 -> "( " ++ unparseApplicationTerm t0 t1 ++ " )"

unparseTerm :: Term -> String
unparseTerm t = case t of
  Variable s i -> unparseVariableTerm s i
  Abstraction s t0 -> unparseAbstractionTerm s t0
  Application t0 t1 -> unparseApplicationTerm t0 t1

unparse :: Term -> String
unparse = unparseTerm

format :: String -> Maybe String
format t = fmap unparse (parse t)
