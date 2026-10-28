module UnoSteve.Secret
  ( Secret (..)
  ) where

import Data.Text (Text)

newtype Secret = Secret {reveal :: Text}

instance Show Secret where
  show _ = "<redacted>"
