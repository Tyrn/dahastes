{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Lib (cmpstrNaturally)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "join" $ do
      it "works" $ do
        T.intercalate "," ["alfa", "bravo"] `shouldBe` "alfa,bravo"
    describe "cmpstrNaturally" $ do
      it "works" $ do
        cmpstrNaturally "" "" `shouldBe` EQ
