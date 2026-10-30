module UnoSteve.Domain.PasswordSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Test.Hspec

import UnoSteve.Domain.Password (mkPassword)

spec :: Spec
spec = describe "mkPassword" $ do
  it "rejects passwords shorter than 10 characters" $
    isLeft (mkPassword "short") `shouldBe` True
  it "accepts a 10-character password" $
    isRight (mkPassword "0123456789") `shouldBe` True
  it "rejects passwords longer than 128 characters" $
    isLeft (mkPassword (T.replicate 129 "a")) `shouldBe` True
