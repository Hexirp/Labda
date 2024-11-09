module Labda.Parser where

import Control.Applicative
import Control.Monad

data ParserResult w s a = Failure w | Success w s a deriving (Eq, Show)

fromParserResultToMaybe :: ParserResult w s a -> Maybe a
fromParserResultToMaybe r = case r of
  Failure w -> Nothing
  Success w s a -> Just a

newtype Parser w s a = Parser { runParser :: s -> ParserResult w s a }

instance Functor (Parser w s) where
  fmap f x0 = Parser $ \s0 -> case runParser x0 s0 of
    Failure w -> Failure w
    Success w s1 x1 -> Success w s1 (f x1)

instance Monoid w => Applicative (Parser w s) where
  pure x = Parser $ \s -> Success mempty s x

  f0 <*> x0 = Parser $ \s0 -> case runParser f0 s0 of
    Failure w0 -> Failure w0
    Success w0 s1 f1 -> case runParser x0 s1 of
      Failure w1 -> Failure (w0 <> w1)
      Success w1 s2 x1 -> Success (w0 <> w1) s2 (f1 x1)

instance Monoid w => Monad (Parser w s) where
  x0 >>= f0 = Parser $ \s0 -> case runParser x0 s0 of
    Failure w0 -> Failure w0
    Success w0 s1 x1 -> case runParser (f0 x1) s1 of
      Failure w1 -> Failure (w0 <> w1)
      Success w1 s2 y1 -> Success (w0 <> w1) s2 y1

instance Monoid w => Alternative (Parser w s) where
  empty = Parser $ \_ -> Failure mempty

  x0 <|> y0 = Parser $ \s0 -> case runParser x0 s0 of
    Failure w0 -> case runParser y0 s0 of
      Failure w1 -> Failure (w0 <> w1)
      Success w1 s2 y1 -> Success (w0 <> w1) s2 y1
    Success w0 s1 x1 -> Success w0 s1 x1

message :: String -> Parser [String] String ()
message m = Parser $ \s -> Success [m] s ()

pop :: Parser [String] String Char
pop = Parser $ \s -> case s of
  [] -> Failure ["pop: it is the end."]
  sh : st -> Success ["pop: it is " ++ show sh ++ "."] st sh

end :: Parser [String] String ()
end = Parser $ \s -> case s of
  [] -> Success ["end: the end matched the end."] s ()
  sh : st -> Failure ["end: " ++ show sh ++ " did not match the end."]

character :: Char -> Parser [String] String ()
character c = Parser $ \s -> case s of
  [] -> Failure ["character: the end did not match " ++ show c ++ "."]
  sh : st -> if sh == c
    then Success ["character: " ++ show sh ++ " matched " ++ show c ++ "."] st ()
    else Failure ["character: " ++ show sh ++ " did not match " ++ show c ++ "."]

symbol :: String -> Parser [String] String ()
symbol s = case s of
  [] -> pure ()
  sh : st -> character sh *> symbol st
