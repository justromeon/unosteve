module UnoSteve.PasswordHasher
  ( Config (..)
  , defaultConfig
  , Handle (..)
  , new
  , verify
  , hash
  , hashWith
  ) where

import Data.Password.Argon2 qualified as Argon2
import Data.Word (Word32)

import Data.Maybe (fromMaybe, isJust)
import UnoSteve.Domain.Password (Password, passwordText)
import UnoSteve.Domain.User (PasswordHash (..), hashText)

-- | How expensive each hash is. Production uses 'defaultConfig' while tests use tiny values.
data Config = Config
  { memoryKiB :: Word32
  , iterations :: Word32
  }
  deriving stock (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { memoryKiB = Argon2.defaultParams.argon2MemoryCost
    , iterations = Argon2.defaultParams.argon2TimeCost
    }

data Handle = Handle
  { params :: Argon2.Argon2Params
  , dummyHash :: PasswordHash
  }

new :: Config -> IO Handle
new cfg = do
  let argonParams =
        Argon2.defaultParams
          { Argon2.argon2MemoryCost = cfg.memoryKiB
          , Argon2.argon2TimeCost = cfg.iterations
          }
  dummy <- hashWith argonParams (Argon2.mkPassword "unosteve timing-equalization dummy")
  pure (Handle argonParams dummy)

-- | Check against dummy hash when no stored for argon2 to still take time
verify :: Handle -> Password -> Maybe PasswordHash -> Bool
verify h password stored =
  case Argon2.checkPassword
    (Argon2.mkPassword $ passwordText password)
    (Argon2.PasswordHash $ hashText $ fromMaybe h.dummyHash stored) of
    Argon2.PasswordCheckSuccess -> isJust stored
    Argon2.PasswordCheckFail -> False

-- | Hash with a fresh random salt (hence IO).
hash :: Handle -> Password -> IO PasswordHash
hash h = hashWith h.params . Argon2.mkPassword . passwordText

hashWith :: Argon2.Argon2Params -> Argon2.Password -> IO PasswordHash
hashWith argonParams plain =
  PasswordHash . Argon2.unPasswordHash <$> Argon2.hashPasswordWithParams argonParams plain
