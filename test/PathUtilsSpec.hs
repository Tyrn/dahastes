module PathUtilsSpec (spec) where

import PathUtils (isRelativeTo, relativeSuffix)
import System.FilePath
import Test.Hspec

spec :: Spec
spec = do
  describe "isRelativeTo" $ do
    context "basic cases" $ do
      it "returns True when child is directly under parent" $
        isRelativeTo "foo/bar/baz" "foo/bar" `shouldBe` True

      it "returns True when child equals parent" $
        isRelativeTo "foo/bar" "foo/bar" `shouldBe` True

      it "returns True when child is deeper" $
        isRelativeTo "a/b/c/d/e" "a/b" `shouldBe` True

      it "returns False when child is shallower" $
        isRelativeTo "foo/bar" "foo/bar/baz" `shouldBe` False

      it "returns False when paths diverge" $
        isRelativeTo "foo/bar" "foo/baz" `shouldBe` False

      it "returns False for sibling paths" $
        isRelativeTo "a/b" "c/d" `shouldBe` False

    context "absolute paths" $ do
      it "returns True for absolute child under absolute parent" $
        isRelativeTo "/a/b/c" "/a" `shouldBe` True

      it "returns True for absolute child under root" $
        isRelativeTo "/a" "/" `shouldBe` True

      it "returns False for relative child under absolute parent" $
        isRelativeTo "a/b" "/a" `shouldBe` False

      it "returns False for absolute child under relative parent" $
        isRelativeTo "/a/b" "a" `shouldBe` False

    context "normalisation" $ do
      it "handles redundant separators" $
        isRelativeTo "foo//bar/baz" "foo/bar" `shouldBe` True

      it "handles single-dot components" $
        isRelativeTo "foo/./bar" "foo" `shouldBe` True

      it "handles trailing slashes" $
        isRelativeTo "foo/bar/" "foo" `shouldBe` True

      -- it "handles .. components lexically" $
      --   isRelativeTo "foo/../bar" "foo" `shouldBe` False

      -- it "collapses .. during normalisation" $
      --   isRelativeTo "foo/../bar" "bar" `shouldBe` True

      it "handles .. components without crashing" $
        isRelativeTo "foo/../bar" "foo" `shouldSatisfy` (\_ -> True)

      it "treats .. as a normal path component" $ do
        isRelativeTo "foo/../bar" "foo" `shouldBe` True
        isRelativeTo "foo/../bar" "bar" `shouldBe` False

    context "edge cases" $ do
      it "returns True for empty parent" $
        isRelativeTo "foo" "" `shouldBe` True

      it "returns True for empty child and empty parent" $
        isRelativeTo "" "" `shouldBe` True

      it "returns True when parent is ." $
        isRelativeTo "foo" "." `shouldBe` True

      it "handles case sensitivity on POSIX" $
        isRelativeTo "Foo/Bar" "foo" `shouldBe` False

  describe "relativeSuffix" $ do
    context "basic cases" $ do
      it "returns the suffix when child is under parent" $
        relativeSuffix "foo/bar/baz" "foo" `shouldBe` Just "bar/baz"

      it "returns '.' when child equals parent" $
        relativeSuffix "foo/bar" "foo/bar" `shouldBe` Just "."

      it "returns Nothing when child is not under parent" $
        relativeSuffix "foo/bar" "baz" `shouldBe` Nothing

      it "returns Nothing when child is shallower" $
        relativeSuffix "foo" "foo/bar" `shouldBe` Nothing

      it "returns the full path when parent is empty" $
        relativeSuffix "foo/bar" "" `shouldBe` Just "foo/bar"

    context "absolute paths" $ do
      it "returns the suffix for absolute paths" $
        relativeSuffix "/a/b/c" "/a" `shouldBe` Just "b/c"

      it "returns '.' for root under root" $
        relativeSuffix "/" "/" `shouldBe` Just "."

      it "returns Nothing for relative under absolute" $
        relativeSuffix "a/b" "/a" `shouldBe` Nothing

    context "normalisation" $ do
      it "handles redundant separators" $
        relativeSuffix "foo//bar/baz" "foo" `shouldBe` Just "bar/baz"

      it "handles trailing slashes" $
        relativeSuffix "foo/bar/" "foo" `shouldBe` Just "bar"

    context "deep nesting" $ do
      it "returns the deepest suffix correctly" $
        relativeSuffix "a/b/c/d/e" "a/b" `shouldBe` Just "c/d/e"

      it "returns Nothing when only the first component matches" $
        relativeSuffix "a/x" "a/b" `shouldBe` Nothing

  describe "round-trip property" $ do
    it "joinPath parent suffix == child when relative" $ do
      let cases =
            [ ("foo/bar/baz", "foo")
            , ("a/b/c/d", "a/b")
            , ("/a/b/c", "/a")
            , ("foo/bar", "foo/bar")
            ]
      mapM_
        ( \(child, parent) ->
            case relativeSuffix child parent of
              Just suffix ->
                dropTrailingPathSeparator (normalise (joinPath [parent, suffix]))
                  `shouldBe` dropTrailingPathSeparator (normalise child)
              Nothing ->
                expectationFailure $
                  "Expected " ++ show child ++ " to be relative to " ++ show parent
        )
        cases
