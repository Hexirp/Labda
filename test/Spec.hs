import Test.Hspec

import Control.Applicative
import Control.Monad

import Labda.Parser
import Labda.UntypedLambdaCalculus

main :: IO ()
main = hspec $ do
  describe "symbol" $ do
    it "is fine" $ do
      runParser (symbol "lambda") "lambda" `shouldBe` Success
        [ IsMatch 'l' 'l'
        , IsMatch 'a' 'a'
        , IsMatch 'm' 'm'
        , IsMatch 'b' 'b'
        , IsMatch 'd' 'd'
        , IsMatch 'a' 'a'
        ]
        ""
        ()
    it "is fine 2" $ do
      runParser (symbol "lambda") "lain" `shouldBe` Failure
        [ IsMatch 'l' 'l'
        , IsMatch 'a' 'a'
        , IsNotMatch (NoEndChar 'i') 'm'
        ]
    it "is fine 3" $ do
      runParser (symbol "lain" <|> symbol "lambda") "lambda" `shouldBe` Success
        [ IsMatch 'l' 'l'
        , IsMatch 'a' 'a'
        , IsNotMatch (NoEndChar 'm') 'i'
        , IsMatch 'l' 'l'
        , IsMatch 'a' 'a'
        , IsMatch 'm' 'm'
        , IsMatch 'b' 'b'
        , IsMatch 'd' 'd'
        , IsMatch 'a' 'a'
        ]
        ""
        ()
    it "is fine 4" $ do
      runParser (symbol "JavaScript") "Java" `shouldBe` Failure
        [ IsMatch 'J' 'J'
        , IsMatch 'a' 'a'
        , IsMatch 'v' 'v'
        , IsMatch 'a' 'a'
        , IsNotMatch End 'S'
        ]
    it "is fine 5" $ do
      runParser (symbol "JavaScript" <|> symbol "Java") "Java" `shouldBe` Success
        [ IsMatch 'J' 'J'
        , IsMatch 'a' 'a'
        , IsMatch 'v' 'v'
        , IsMatch 'a' 'a'
        , IsNotMatch End 'S'
        , IsMatch 'J' 'J'
        , IsMatch 'a' 'a'
        , IsMatch 'v' 'v'
        , IsMatch 'a' 'a'
        ]
        ""
        ()
    it "is fine 6" $ do
      runParser (symbol "Java" <|> symbol "JavaScript") "JavaScript" `shouldBe` Success
        [ IsMatch 'J' 'J'
        , IsMatch 'a' 'a'
        , IsMatch 'v' 'v'
        , IsMatch 'a' 'a'
        ]
        "Script"
        ()
  describe "format" $ do
    it "returns the formatted code" $ do
      format "lambda x => lambda y => x#1" `shouldBe` "lambda x => lambda y => x#1"
