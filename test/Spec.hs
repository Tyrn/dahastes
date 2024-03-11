{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Lib (cmpstrNaturally, initials, isSomeText, makeInitials, removeQuotedSubstrings, splitOnDots)
import Test.Hspec
import Text.Regex.TDFA

main :: IO ()
main =
  hspec $ do
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
    describe "splinOnDots" $ do
      it "works" $ do
        splitOnDots "a . .. b c" `shouldBe` ["a", "b", "c"]
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
    describe "Join miscellany" $ do
      it "works" $ do
        isSomeText "" `shouldBe` False
        isSomeText "z" `shouldBe` True
        isSomeText "\x2698" `shouldBe` True
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
        initials "" `shouldBe` ""
        initials " " `shouldBe` ""
        initials ".. , .. " `shouldBe` ""
        initials " ,, .," `shouldBe` ""
        initials " ,, . -," `shouldBe` ""
        -- initials ", a. g, " `shouldBe` "A.G."
        initials "- , -I.V.-A,E.C.N-, ." `shouldBe` "I.V-A.,E.C.N."
        -- initials "John ronald reuel Tolkien" `shouldBe` "J.R.R.T."
        -- initials "  e.B.Sledge " `shouldBe` "E.B.S."
        initials "Apsley Cherry-Garrard" `shouldBe` "A.C-G."
        initials "Windsor Saxe-\tCoburg - Gotha" `shouldBe` "W.S-C-G."
        initials "Elisabeth Kubler-- - Ross" `shouldBe` "E.K-R."
        initials "  Fitz-Simmons Ashton-Burke Leigh" `shouldBe` "F-S.A-B.L."
        initials "Arleigh \"31-knot\"Burke " `shouldBe` "A.B."
        initials "Harry \"Bing\" Crosby, Kris \"Tanto\" Paronto" `shouldBe` "H.C.,K.P."
        initials "William J. \"Wild Bill\" Donovan, Marta \"Cinta Gonzalez" `shouldBe` "W.J.D.,M.C.G."
        -- initials "a.s , - . ,b.s." `shouldBe` "A.S.,B.S."
        initials "A. Strugatsky, B...Strugatsky." `shouldBe` "A.S.,B.S."
        initials "Иржи Кропачек,, Йозеф Новотный" `shouldBe` "И.К.,Й.Н."
        -- initials "Язон динАльт, Шарль д'Артаньян" `shouldBe` "Я.динА.,Ш.д'А."
        -- initials "Charles de Batz de Castelmore d'Artagnan" `shouldBe` "C.d.B.d.C.d'A."
        initials "Mario Del Monaco, Hutchinson of London" `shouldBe` "M.D.M.,H.o.L."
        initials "Anselm haut Rodric" `shouldBe` "A.h.R."
        initials "Ансельм от Родрик" `shouldBe` "А.о.Р."
        -- initials "Leonardo Wilhelm DiCaprio" `shouldBe` "L.W.DiC."
        -- initials "леонардо вильгельм ди каприо" `shouldBe` "Л.В.д.К."
        -- initials "kapitän zur see" `shouldBe` "K.z.S."
        initials "De Beers, Guido van Rossum" `shouldBe` "D.B.,G.v.R."
        initials "Манфред фон Рихтгофен" `shouldBe` "М.ф.Р."
        initials "Armand Jean du Plessis" `shouldBe` "A.J.d.P."
        -- initials "johannes diderik van der waals" `shouldBe` "J.D.v.d.W."
        initials "Karl Hård af Segerstad" `shouldBe` "K.H.a.S."
        -- initials "Österreich über alles" `shouldBe` "Ö.Ü.A."
        initials "José Eduardo dos Santos" `shouldBe` "J.E.d.S."
        -- initials "Gnda'Ke" `shouldBe` "Gnda'K."
        -- initials "gnda'ke" `shouldBe` "G."
        -- initials "gnda'" `shouldBe` "G."
        -- initials "'Bravo" `shouldBe` "'B."
        initials "'" `shouldBe` "'."
        -- initials "'B" `shouldBe` "'B."
        -- initials "'b" `shouldBe` "'b."
        -- initials "dA" `shouldBe` "dA."
        -- initials "DA" `shouldBe` "DA."
        -- initials "DAMadar" `shouldBe` "DA."
        -- initials "Плиний Старший" `shouldBe` "П.Ст."
        -- initials "Плиний Младший" `shouldBe` "П.Мл."
        -- initials "Плиний Мл." `shouldBe` "П.Мл."
        -- initials "George Smith Patton Jr." `shouldBe` "G.S.P.Jr."
        -- initials "Джордж Смит паттон ст" `shouldBe` "Д.С.П.ст."
        -- initials "Redington Sr" `shouldBe` "R.Sr."
        initials "Pliny the Elder" `shouldBe` "P.t.E."
