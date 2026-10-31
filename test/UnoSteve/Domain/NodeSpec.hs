module UnoSteve.Domain.NodeSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Data.UUID (nil)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Support (testName)
import UnoSteve.Domain.Node

spec :: Spec
spec = do
  describe "mkNodeName" $ do
    it "keeps an ordinary name as is, spaces and unicode included" $
      nodeNameText <$> mkNodeName "Résumé 2026.pdf" `shouldBe` Right "Résumé 2026.pdf"

    it "counts characters, not bytes: 255 é pass, 256 characters fail" $ do
      isRight (mkNodeName $ T.replicate 255 "\233") `shouldBe` True
      isLeft (mkNodeName $ T.replicate 256 "a") `shouldBe` True

    it "it rejects empty, . and .." $
      all (isLeft . mkNodeName) ["", ".", ".."] `shouldBe` True

    it "rejects a slash" $
      isLeft (mkNodeName "a/b") `shouldBe` True

    it "rejects control characters such as NUL and newline" $
      all (isLeft . mkNodeName) ["a\NULb", "a\nb"] `shouldBe` True

  describe "Cursor" $ do
    it "survives a rount trip through its text form" $ do
      let cursor = Cursor{isFile = True, name = testName "Résumé.pdf", nodeId = NodeId nil}
      parseCursor (cursorText cursor) `shouldBe` Right cursor

    it "rejects a text that isn't base64yrl" $
      isLeft (parseCursor "not a cursor") `shouldBe` True

    it "rejects a valid base64url but not a cursor" $
      isLeft (parseCursor "e30") `shouldBe` True -- base64url of "{}"
