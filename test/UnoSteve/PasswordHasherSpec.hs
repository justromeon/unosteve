module UnoSteve.PasswordHasherSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Test.Support (testHasherConfig, testPassword)
import UnoSteve.Domain.User (hashText)
import UnoSteve.PasswordHasher qualified as PasswordHasher

spec :: Spec
spec = do
  it "stores the algorithm and its parameters with the hash" $ do
    hasher <- PasswordHasher.new testHasherConfig
    stored <- PasswordHasher.hash hasher (testPassword "correct horse battery")
    T.isPrefixOf "$argon2id$v=19$m=8,t=1,p=1$" (hashText stored) `shouldBe` True

  it "rejects every password when there is no stored hash" $ do
    hasher <- PasswordHasher.new testHasherConfig
    PasswordHasher.verify hasher "unosteve timing-equalization dummy" Nothing
      `shouldBe` False
