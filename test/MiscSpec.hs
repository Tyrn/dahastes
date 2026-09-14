{-# LANGUAGE OverloadedStrings #-}

module MiscSpec (spec) where

import Data.Text qualified as T
import Lib (cmpstrNaturally, humanFine)
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

  describe "humanFine" $ do
    context "small values" $ do
      it "returns '0' for 0" $
        humanFine 0 `shouldBe` "0"

      it "returns '1' for 1" $
        humanFine 1 `shouldBe` "1"

      it "returns '42' for 42" $
        humanFine 42 `shouldBe` "42"

    context "kilobytes" $ do
      it "returns '2kB' for 1800" $
        humanFine 1800 `shouldBe` "2kB"

      it "returns '1kB' for 1024" $
        humanFine 1024 `shouldBe` "1kB"

    context "megabytes" $ do
      it "returns '117.7MB' for 123456789" $
        humanFine 123456789 `shouldBe` "117.7MB"

      it "returns '1.0MB' for 1024^2" $
        humanFine (1024 ^ (2 :: Int)) `shouldBe` "1.0MB"

    context "gigabytes" $ do
      it "returns '114.98GB' for 123456789123" $
        humanFine 123456789123 `shouldBe` "114.98GB"

      it "returns '1.00GB' for 1024^3" $
        humanFine (1024 ^ (3 :: Int)) `shouldBe` "1.00GB"

    context "terabytes" $ do
      it "returns '1.00TB' for 1024^4" $
        humanFine (1024 ^ (4 :: Int)) `shouldBe` "1.00TB"

    context "petabytes" $ do
      it "returns '1.00PB' for 1024^5" $
        humanFine (1024 ^ (5 :: Int)) `shouldBe` "1.00PB"

    context "exabytes and beyond" $ do
      it "returns '1.00EB' for 1024^6" $
        humanFine (1024 ^ (6 :: Int)) `shouldBe` "1.00EB"

      it "returns '1.00ZB' for 1024^7" $
        humanFine (1024 ^ (7 :: Int)) `shouldBe` "1.00ZB"

      it "returns '1.00YB' for 1024^8" $
        humanFine (1024 ^ (8 :: Int)) `shouldBe` "1.00YB"

      it "clamps to '1024.00YB' for 1024^9 (no larger unit)" $
        humanFine (1024 ^ (9 :: Int)) `shouldBe` "1024.00YB"
