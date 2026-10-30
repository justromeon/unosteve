module Main (main) where

import Test.Hspec

import UnoSteve.AuthSpec qualified
import UnoSteve.Database.UsersSpec qualified
import UnoSteve.Domain.PasswordSpec qualified
import UnoSteve.Domain.UserSpec qualified
import UnoSteve.LoggerSpec qualified
import UnoSteve.PasswordHasherSpec qualified
import UnoSteve.SessionsSpec qualified

main :: IO ()
main = hspec $ do
  UnoSteve.LoggerSpec.spec
  UnoSteve.Domain.UserSpec.spec
  describe "Database.Users" UnoSteve.Database.UsersSpec.spec
  UnoSteve.Domain.PasswordSpec.spec
  describe "PasswordHasher" UnoSteve.PasswordHasherSpec.spec
  describe "Sessions" UnoSteve.SessionsSpec.spec
  describe "Auth" UnoSteve.AuthSpec.spec
