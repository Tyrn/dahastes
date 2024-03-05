import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "dahastes-test" $
      it "works" $
        2 + 2 `shouldBe` (4 :: Int)
