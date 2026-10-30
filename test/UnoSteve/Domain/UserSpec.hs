module UnoSteve.Domain.UserSpec (spec) where

import Data.Either (isLeft)
import Test.Hspec

import UnoSteve.Domain.User (emailText, mkEmail)

spec :: Spec
spec = do
  describe "mkEmail" $ do
    it "trims and lowercases" $
      emailText <$> mkEmail "  Justin@Example.COM " `shouldBe` Right "justin@example.com"
    it "rejects a missing @" $
      isLeft (mkEmail "justin.example.com") `shouldBe` True
    it "rejects more than one @" $
      isLeft (mkEmail "a@b@c") `shouldBe` True
