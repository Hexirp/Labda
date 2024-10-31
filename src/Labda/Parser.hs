module Labda.Parser where

import Control.Applicative
import Control.Monad

data ParserResult w s a = Failure w s | Success w s a deriving (Eq, Show)

newtype Parser w s a = Parser { runParser :: s -> ParserResult w s a }

instance Functor (Parser w s) where
  fmap f x0 = Parser $ \s0 -> case runParser x0 s0 of
    Failure w s1 -> Failure w s1
    Success w s1 x1 -> Success w s1 (f x1)

instance Monoid w => Applicative (Parser w s) where
  pure x = Parser $ \s -> Success mempty s x

  f0 <*> x0 = Parser $ \s0 -> case runParser f0 s0 of
    Failure w0 s1 -> Failure w0 s1
    Success w0 s1 f1 -> case runParser x0 s1 of
      Failure w1 s2 -> Failure (w0 <> w1) s2
      Success w1 s2 x1 -> Success (w0 <> w1) s2 (f1 x1)

instance Monoid w => Monad (Parser w s) where
  x0 >>= f0 = Parser $ \s0 -> case runParser x0 s0 of
    Failure w0 s1 -> Failure w0 s1
    Success w0 s1 x1 -> case runParser (f0 x1) s1 of
      Failure w1 s2 -> Failure (w0 <> w1) s2
      Success w1 s2 y1 -> Success (w0 <> w1) s2 y1

instance Monoid w => Alternative (Parser w s) where
  empty = Parser $ \s -> Failure mempty s

  x0 <|> y0 = Parser $ \s0 -> case runParser x0 s0 of
    Failure w0 s1 -> case runParser y0 s0 of
      Failure w1 s2 -> Failure (w0 <> w1) s2
      Success w1 s2 y1 -> Success (w0 <> w1) s2 y1
    Success w0 s1 x1 -> Success w0 s1 x1

instance Monoid w => MonadPlus (Parser w s) where

character :: Char -> Parser String String ()
character c = Parser $ \s -> case s of
  [] -> Failure "it is ended\n" s
  sh : st -> if c == sh
    then Success ("'" ++ [c] ++ "' is detected\n") st ()
    else Failure ("'" ++ [sh] ++ "' is not '" ++ [c] ++ "'\n") s

symbol :: String -> Parser String String ()
symbol [] = pure ()
symbol (sh : st) = character sh >> symbol st
