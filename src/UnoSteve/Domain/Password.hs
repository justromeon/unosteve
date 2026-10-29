module UnoSteve.Domain.Password
  ( Password
  , mkPassword
  , passwordText
  ) where

import Data.Text (Text)
import Data.Text qualified as T

newtype Password = Password Text

instance Show Password where
  show _ = "<password>"

mkPassword :: Text -> Either Text Password
mkPassword raw
  | len < 10 = Left "Password must be at least 10 characters."
  | len > 128 = Left "Password must be at most 128 characters."
  | otherwise = Right (Password raw)
 where
  len = T.length raw

passwordText :: Password -> Text
passwordText (Password t) = t
