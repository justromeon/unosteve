module Main (main) where

import UnoSteve.Api (api, server)

import Network.Wai.Handler.Warp (run)
import Servant

main :: IO ()
main = do
  putStrLn "Server running"
  run 8080 (serve api server)
