module Labda.Parser where

import Control.Applicative
import Control.Monad

data ParserResult w s a = Failure w | Success w s a deriving (Eq, Show)

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

data CharWithEnd = NoEndChar Char | End deriving (Eq, Show)

data ParserLogMessage = IsMatch Char Char | IsNotMatch CharWithEnd Char deriving (Eq, Show)

character :: Char -> Parser [ParserLogMessage] String ()
character c = Parser $ \s -> case s of
  [] -> Failure [IsNotMatch End c]
  sh : st -> if sh == c
    then Success [IsMatch sh c] st ()
    else Failure [IsNotMatch (NoEndChar sh) c]

symbol :: String -> Parser [ParserLogMessage] String ()
symbol [] = pure ()
symbol (sh : st) = character sh >> symbol st

end :: Parser [ParserLogMessage] String ()
end = Parser $ \s -> if s == [] then Success [] s () else Failure []
