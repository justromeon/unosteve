module UnoSteve.Domain.Page
  ( PageSize
  , mkPageSize
  , pageSizeInt
  , maxPageSize
  , defaultPageSize
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

newtype PageSize = PageSize Int
  deriving newtype (Eq, Show)

instance FromHttpApiData PageSize where
  parseUrlPiece t = parseUrlPiece t >>= mkPageSize

mkPageSize :: Int -> Either Text PageSize
mkPageSize n
  | n >= 1 && n <= maxPageSize = Right (PageSize n)
  | otherwise = Left ("limit must be between 1 and " <> T.pack (show maxPageSize))

pageSizeInt :: PageSize -> Int
pageSizeInt (PageSize n) = n

maxPageSize :: Int
maxPageSize = 100

defaultPageSize :: PageSize
defaultPageSize = PageSize 50
