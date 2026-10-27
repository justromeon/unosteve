module UnoSteve.UserRepo where

import UnoSteve.User (NewUser, Email, User, UserId)

data CreateError = EmailTaken
  deriving (Eq, Show)

data Handle = Handle
  { create :: NewUser -> IO (Either CreateError User)
  , findByEmail :: Email -> IO (Maybe User)
  , findById :: UserId -> IO (Maybe User)
  }
