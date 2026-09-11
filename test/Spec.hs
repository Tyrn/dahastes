{-# LANGUAGE OverloadedStrings #-}

import InitialsSpec qualified
import MiscSpec qualified
import PathUtilsSpec qualified
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "MiscSpec" MiscSpec.spec
    describe "InitialsSpec" InitialsSpec.spec
    describe "PathUtilsSpec" PathUtilsSpec.spec
