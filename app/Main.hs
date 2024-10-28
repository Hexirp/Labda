module Main ( main ) where

import Labda.UntypedLambdaCalculus

main :: IO ()
main = putStrLn ( unparse ( Abstraction "x" ( Variable "x" 0 ) ) )
