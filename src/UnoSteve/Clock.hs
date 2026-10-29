module UnoSteve.Clock
  ( Handle (..)
  ) where

import Data.Time (UTCTime)

newtype Handle = Handle {now :: IO UTCTime}
