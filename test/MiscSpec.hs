{-# LANGUAGE OverloadedStrings #-}

module MiscSpec (spec) where

import Data.Text qualified as T
import Lib (cmpstrNaturally)
import Test.Hspec
import Text.Regex.TDFA

spec :: Spec
spec = do
  describe "Join miscellany" $ do
    it "works" $ do
      T.intercalate "-" ["alfa", "bravo"] `shouldBe` "alfa-bravo"
      T.intercalate " " (T.splitOn "\"" "\"Morro\"Castle\"Bridge\"") `shouldBe` " Morro Castle Bridge "
      T.splitOn "'" "" `shouldBe` [""]
      T.splitOn "'" "a" `shouldBe` ["a"]
      T.splitOn "'" "'a" `shouldBe` ["", "a"]
      T.splitOn "'" "a'" `shouldBe` ["a", ""]
      concat (("a . .. b c" :: String) =~ ("[\\s.]+" :: String) :: [[String]]) `shouldBe` [".", ".."]
      concat (("a . .. b c" :: String) =~ ("[^s.]+" :: String) :: [[String]]) `shouldBe` ["a ", " ", " b c"]
  describe "cmpstrNaturally" $ do
    it "works" $ do
      cmpstrNaturally "" "" `shouldBe` (EQ :: Ordering)
      cmpstrNaturally "" "a" `shouldBe` LT
      cmpstrNaturally "2a" "10a" `shouldBe` LT
      cmpstrNaturally "alfa" "bravo" `shouldBe` LT
