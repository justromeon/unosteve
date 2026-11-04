module Main (main) where

import Test.Hspec

import UnoSteve.ApiSpec qualified
import UnoSteve.AuthSpec qualified
import UnoSteve.Database.NodesSpec qualified
import UnoSteve.Database.UsersSpec qualified
import UnoSteve.Domain.NodeSpec qualified
import UnoSteve.Domain.PageSpec qualified
import UnoSteve.Domain.PasswordSpec qualified
import UnoSteve.Domain.UserSpec qualified
import UnoSteve.LoggerSpec qualified
import UnoSteve.NodesSpec qualified
import UnoSteve.PasswordHasherSpec qualified
import UnoSteve.SessionsSpec qualified
import UnoSteve.Storage.R2.SigV4Spec qualified
import UnoSteve.Storage.R2Spec qualified
import UnoSteve.StorageSpec qualified

main :: IO ()
main = hspec $ do
  UnoSteve.LoggerSpec.spec
  UnoSteve.Domain.UserSpec.spec
  describe "Database.Users" UnoSteve.Database.UsersSpec.spec
  UnoSteve.Domain.PasswordSpec.spec
  describe "PasswordHasher" UnoSteve.PasswordHasherSpec.spec
  describe "Sessions" UnoSteve.SessionsSpec.spec
  describe "Auth" UnoSteve.AuthSpec.spec
  describe "Api" UnoSteve.ApiSpec.spec
  UnoSteve.Domain.NodeSpec.spec
  UnoSteve.Domain.PageSpec.spec
  describe "Database.Nodes" UnoSteve.Database.NodesSpec.spec
  describe "Nodes" UnoSteve.NodesSpec.spec
  describe "Storage" UnoSteve.StorageSpec.spec
  describe "Storage.R2" UnoSteve.Storage.R2Spec.spec
  describe "SigV4" UnoSteve.Storage.R2.SigV4Spec.spec
