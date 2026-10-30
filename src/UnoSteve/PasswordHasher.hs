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
import Data.Text (Text)
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

{- | Check against dummy hash when no stored so that argon2 will still take time.
The attempt is plain Text, deliberately not parsed with mkPassword: if the password
policy ever gets stricter (say a longer minimum), passwords created under the old
policy must keep working
-}
verify :: Handle -> Text -> Maybe PasswordHash -> Bool
verify h attempt stored =
  case Argon2.checkPassword
    (Argon2.mkPassword attempt)
    (Argon2.PasswordHash $ hashText $ fromMaybe h.dummyHash stored) of
    Argon2.PasswordCheckSuccess -> isJust stored
    Argon2.PasswordCheckFail -> False

-- | Hash with a fresh random salt (hence IO).
hash :: Handle -> Password -> IO PasswordHash
hash h = hashWith h.params . Argon2.mkPassword . passwordText

hashWith :: Argon2.Argon2Params -> Argon2.Password -> IO PasswordHash
hashWith argonParams plain =
  PasswordHash . Argon2.unPasswordHash <$> Argon2.hashPasswordWithParams argonParams plain
