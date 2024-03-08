import Lib (cmpstrNaturally)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "add" $ do
      it "works" $ do
        2 + 2 `shouldBe` (4 :: Int)
    describe "cmpstrNaturally" $ do
      it "works" $ do
        cmpstrNaturally "" "" `shouldBe` EQ
