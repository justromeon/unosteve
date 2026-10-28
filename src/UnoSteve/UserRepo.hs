module UnoSteve.UserRepo
  ( CreateError (..)
  , Handle (..)
  ) where

import UnoSteve.Domain.User (Email, NewUser, User, UserId)

data CreateError = EmailTaken
  deriving (Eq, Show)

data Handle = Handle
  { create :: NewUser -> IO (Either CreateError User)
  , findByEmail :: Email -> IO (Maybe User)
  , findById :: UserId -> IO (Maybe User)
  }
