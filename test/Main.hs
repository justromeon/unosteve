module Main (main) where

import Data.IORef (newIORef, readIORef, modifyIORef')
import Test.Hspec

import UnoSteve.Logger qualified as Logger

main :: IO ()
main = hspec $ do
  describe "Logger.atLeast" $ do
    it "drops a message below threshold" $ do
      ref <- newIORef []
      let collecting = Logger.Handle $ \level msg -> modifyIORef' ref ((level, msg) :)
          logger = Logger.atLeast Logger.Warn collecting

      Logger.logDebug logger "hidden"
      Logger.logError logger "shown"
      readIORef ref `shouldReturn` [(Logger.Error, "shown")]

    it "keeps a message within threshold" $ do
      ref <- newIORef []
      let collecting = Logger.Handle $ \level msg -> modifyIORef' ref ((level, msg) :)
          logger = Logger.atLeast Logger.Debug collecting

      Logger.logDebug logger "shown"
      Logger.logError logger "shown"
      readIORef ref `shouldReturn` [(Logger.Error, "shown"), (Logger.Debug, "shown")]

  describe "Logger.withPrefix" $ do
    it "prepends a prefix on logs" $ do
      ref <- newIORef []
      let collecting = Logger.Handle $ \level msg -> modifyIORef' ref ((level, msg) :)
          logger = Logger.withPrefix "[api] " collecting

      Logger.logInfo logger "prefixed log test"
      readIORef ref `shouldReturn` [(Logger.Info, "[api] prefixed log test")]
