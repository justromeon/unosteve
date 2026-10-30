module UnoSteve.AuthSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Test.Support (postgresSpec, sessionConfig, t0, testEmail, testHasherConfig, testPassword)
import UnoSteve.Auth qualified as Auth
import UnoSteve.Clock.Fixed qualified as Clock.Fixed
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Session qualified as Domain.Session
import UnoSteve.Domain.User (User (..), hashText)
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions

spec :: Spec
spec = postgresSpec (beforeWith setup authSpec)

setup :: Database.Handle -> IO (Database.Handle, PasswordHasher.Handle, Sessions.Handle)
setup db = do
  hasher <- PasswordHasher.new testHasherConfig
  let sessions = Sessions.new sessionConfig db (Clock.Fixed.new t0)
  pure (db, hasher, sessions)

authSpec :: SpecWith (Database.Handle, PasswordHasher.Handle, Sessions.Handle)
authSpec = do
  describe "register" $ do
    it "stores an argon2 hash, never the plain password" $ \(db, hasher, _) -> do
      Right user <- Auth.register hasher db justin correctHorse
      T.isPrefixOf "$argon2id$" (hashText user.passwordHash) `shouldBe` True

    it "rejects a duplicate email" $ \(db, hasher, _) -> do
      _ <- Auth.register hasher db justin correctHorse
      Auth.register hasher db (testEmail "Justin@Example.com") (testPassword "another long password")
        `shouldReturn` Left Database.Users.EmailTaken

  describe "login" $ do
    it "starts a session for the right password" $ \(db, hasher, sessions) -> do
      Right user <- Auth.register hasher db justin correctHorse
      Right (token, _) <- Auth.login hasher db sessions justin "correct horse battery"
      fmap Domain.Session.userId <$> Sessions.authenticate sessions token `shouldReturn` Just user.userId

    it "gives every failure the same error" $ \(db, hasher, sessions) -> do
      _ <- Auth.register hasher db justin correctHorse
      let attempts =
            [ (justin, "wrong horse battery") -- wrong password
            , (testEmail "nobody@example.com", "correct horse battery") -- unknown email
            , (justin, "") -- empty password: checked, not rejected by policy
            ]
      results <- traverse (uncurry (Auth.login hasher db sessions)) attempts
      -- Note: SessionToken has no Eq instance
      [either Just (const Nothing) r | r <- results]
        `shouldBe` replicate (length attempts) (Just Auth.InvalidCredentials)
 where
  justin = testEmail "justin@example.com"
  correctHorse = testPassword "correct horse battery"
