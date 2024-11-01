import Test.Hspec

import Control.Applicative
import Control.Monad

import Labda.Parser
import Labda.UntypedLambdaCalculus

main :: IO ()
main = hspec $ do
  describe "symbol" $ do
    it "is fine" $ do
      runParser (symbol "lambda") "lambda" `shouldBe` Success [] "" ()
    it "is fine 2" $ do
      runParser (symbol "lambda") "lain" `shouldBe` Failure []
    it "is fine 3" $ do
      runParser (symbol "lain" <|> symbol "lambda") "lambda" `shouldBe` Success [] "" ()
  describe "format" $ do
    it "returns the formatted code" $ do
      format "lambda x => lambda y => x#1" `shouldBe` "lambda x => lambda y => x#1"
