module UnoSteve.Domain.PageSpec (spec) where

import Data.Either (isLeft)
import Data.Text
import Test.Hspec (Spec, describe, it, shouldBe)
import UnoSteve.Domain.Page (PageSize, mkPageSize, pageSizeInt)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

spec :: Spec
spec = do
  describe "PageSize" $ do
    it "accepts 1 to 100" $
      (pageSizeInt <$> mkPageSize 1, pageSizeInt <$> mkPageSize 100) `shouldBe` (Right 1, Right 100)

    it "rejects 0 and 101" $
      (isLeft (mkPageSize 0), isLeft (mkPageSize 101)) `shouldBe` (True, True)

    it "parses a query parameter" $ do
      pageSizeInt <$> parseUrlPiece "20" `shouldBe` Right 20
      isLeft (parseUrlPiece "abc" :: Either Text PageSize) `shouldBe` True
