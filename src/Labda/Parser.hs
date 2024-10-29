module Labda.Parser where

newtype Parser s a = Parser { runParser :: s -> (a, s) }

sequence :: (a -> Either c (b -> c)) -> Parser s a -> Parser s b -> Parser s c
sequence f x y = Parser $ \s -> case runParser x s of
  (a, s') -> case f a of
    Left c -> (c, s')
    Right g -> case runParser y s' of
      (b, s'') -> (g b, s'')
