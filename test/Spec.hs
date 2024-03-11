{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Lib (cmpstrNaturally, initials, makeInitials, removeQuotedSubstrings)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Join miscellany" $ do
      it "works" $ do
        T.intercalate "-" ["alfa", "bravo"] `shouldBe` "alfa-bravo"
        T.splitOn "-" "alfa-bravo" `shouldBe` ["alfa", "bravo"]
        T.intercalate " " (T.splitOn "\"" "\"Morro\"Castle\"Bridge\"") `shouldBe` " Morro Castle Bridge "
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
        removeQuotedSubstrings "\"Bing\"Crosby, Kris\"Tanto Paronto" `shouldBe` " Crosby, Kris Tanto Paronto"
    describe "makeInitials" $ do
      it "works" $ do
        makeInitials "Arleigh\"31-knot\"Burke" `shouldBe` "A.B."
        makeInitials "\"Bing\"Crosby, Kris\"Tanto\"Paronto" `shouldBe` "C.K.P."
        makeInitials "\"Bing\"Crosby, Kris\"Tanto Paronto" `shouldBe` "C.K.T.P."
        makeInitials "" `shouldBe` "."
        makeInitials "" `shouldBe` "."
        makeInitials "Elisabeth Kubler-- - Ross" `shouldBe` "E.K---R."
        makeInitials "Fitz-Simmons\tAshton-Burke Leigh" `shouldBe` "F-S.A-B.L."
    describe "initials" $ do
      it "works" $ do
        initials "Arleigh\"31-knot\"Burke" `shouldBe` "A.B."
        initials "\"Bing\"Crosby, Kris\"Tanto\"Paronto" `shouldBe` "C.K.P."
        initials "\"Bing\"Crosby, Kris\"Tanto Paronto" `shouldBe` "C.K.T.P."
        initials "" `shouldBe` "."
        initials "" `shouldBe` "."
        initials "Elisabeth Kubler-- - Ross" `shouldBe` "E.K---R."
        initials "Fitz-Simmons\tAshton-Burke Leigh" `shouldBe` "F-S.A-B.L."
