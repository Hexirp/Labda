import Test.Hspec

import Control.Applicative

import Labda.Parser
import Labda.UntypedLambdaCalculus

main :: IO ()
main = hspec $ do
  describe "symbol" $ do
    it "is fine" $ do
      fromParserResultToMaybe (runParser (symbol "lambda") "lambda") `shouldBe` Just ()
    it "is fine 2" $ do
      fromParserResultToMaybe (runParser (symbol "lambda") "lain") `shouldBe` Nothing
    it "is fine 3" $ do
      fromParserResultToMaybe (runParser (symbol "lain" <|> symbol "lambda") "lambda") `shouldBe` Just ()
    it "is fine 4" $ do
      fromParserResultToMaybe (runParser (symbol "JavaScript") "Java") `shouldBe` Nothing
    it "is fine 5" $ do
      fromParserResultToMaybe (runParser (symbol "JavaScript" <|> symbol "Java") "Java") `shouldBe` Just ()
    it "is fine 6" $ do
      fromParserResultToMaybe (runParser (symbol "Java" <|> symbol "JavaScript") "JavaScript") `shouldBe` Just ()
    it "is fine 7" $ do
      fromParserResultToMaybe (runParser (symbol "Java" *> end <|> symbol "JavaScript" *> end) "JavaScript") `shouldBe` Just ()
  describe "format" $ do
    it "is fine" $ do
      format "lambda x => lambda y => x#1" `shouldBe` Just "lambda x => lambda y => x#1"
    it "is fine 2" $ do
      format "lambda x => ( lambda y => x#1 ( y#0 y#0 ) ) ( lambda y => x#1 ( y#0 y#0 ) )"
        `shouldBe` Just "lambda x => ( lambda y => x#1 ( y#0 y#0 ) ) ( lambda y => x#1 ( y#0 y#0 ) )"
