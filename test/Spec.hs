{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Lib (cmpstrNaturally, removeQuotedSubstrings)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "join" $ do
      it "works" $ do
        T.intercalate "-" ["alfa", "bravo"] `shouldBe` "alfa-bravo"
        T.splitOn "-" "alfa-bravo" `shouldBe` ["alfa", "bravo"]
    describe "cmpstrNaturally" $ do
      it "works" $ do
        cmpstrNaturally "" "" `shouldBe` (EQ :: Ordering)
        cmpstrNaturally "" "a" `shouldBe` LT
        cmpstrNaturally "2a" "10a" `shouldBe` LT
        cmpstrNaturally "alfa" "bravo" `shouldBe` LT
    describe "removeQuotedSubstrings" $ do
      it "works" $ do
        removeQuotedSubstrings "" `shouldBe` ""
        removeQuotedSubstrings "Arleigh\"31-knot\"Burke" `shouldBe` "Arleigh Burke"
        removeQuotedSubstrings "\"Bing\"Crosby, Kris\"Tanto\"Paronto" `shouldBe` " Crosby, Kris Paronto"
