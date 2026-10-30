module UnoSteve.LoggerSpec (spec) where

import Data.IORef (newIORef, readIORef)
import Test.Hspec

import UnoSteve.Logger qualified as Logger
import UnoSteve.Logger.Collecting qualified as Logger.Collecting

spec :: Spec
spec = do
  describe "Logger.atLeast" $ do
    it "drops a message below threshold" $ do
      logged <- newIORef []
      let logger = Logger.atLeast Logger.Warn (Logger.Collecting.new logged)
      Logger.logDebug logger "hidden"
      Logger.logError logger "shown"
      readIORef logged `shouldReturn` [(Logger.Error, "shown")]

    it "keeps a message within threshold" $ do
      logged <- newIORef []
      let logger = Logger.atLeast Logger.Debug (Logger.Collecting.new logged)
      Logger.logDebug logger "shown"
      Logger.logError logger "shown"
      readIORef logged `shouldReturn` [(Logger.Debug, "shown"), (Logger.Error, "shown")]

  describe "Logger.withPrefix" $ do
    it "prepends a prefix on logs" $ do
      logged <- newIORef []
      let logger = Logger.withPrefix "[api] " (Logger.Collecting.new logged)
      Logger.logInfo logger "prefixed log test"
      readIORef logged `shouldReturn` [(Logger.Info, "[api] prefixed log test")]
