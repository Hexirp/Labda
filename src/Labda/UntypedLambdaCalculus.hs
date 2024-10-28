module Labda.UntypedLambdaCalculus ( Tree ( .. ) , parse , unparse ) where

data Tree = Variable String Word | Abstraction String Tree | Application Tree Tree

parse :: String -> Tree
parse = undefined

unparse :: Tree -> String
unparse ( Variable name index ) = name ++ "#" ++ show index
unparse ( Abstraction name tree ) = "(" ++ " " ++ "lambda" ++ " " ++ name ++ " " ++ "=>" ++ " " ++ unparse tree ++ " " ++ ")"
unparse ( Application tree1 tree2 ) = "(" ++ " " ++ unparse tree1 ++ " " ++ unparse tree2 ++ " " ++ ")"
