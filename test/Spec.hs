import Test.Hspec

import Labda.UntypedLambdaCalculus

main :: IO ()
main = hspec $ do
    describe "format" $ do
        it "returns the formatted code" $ do
            format "lambda x => lambda y => x#1" `shouldBe` "lambda x => lambda y => x#1"
