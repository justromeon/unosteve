module UnoSteve.Logger.Noop
  ( new
  ) where

import UnoSteve.Logger qualified as Logger

-- | Discards everything. For code paths where logging doesn't matter.
new :: Logger.Handle
new = Logger.Handle (\_ _ -> pure ())
