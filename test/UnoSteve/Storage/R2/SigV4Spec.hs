module UnoSteve.Storage.R2.SigV4Spec (spec) where

import Data.Time (UTCTime (UTCTime), fromGregorian)
import Test.Hspec (Spec, it, shouldBe)
import Test.Support (t0)
import UnoSteve.Secret (Secret (Secret))
import UnoSteve.Storage.R2.SigV4 qualified as SigV4

-- | The example key pair from AWS's documentation.
exampleCredentials :: SigV4.Credentials
exampleCredentials =
  SigV4.Credentials
    { SigV4.accessKey = "AKIAIOSFODNN7EXAMPLE"
    , SigV4.secretKey = Secret "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
    }

spec :: Spec
spec = do
  -- https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html
  it "reproduces AWS's published example" $
    SigV4.presignUrl
      exampleCredentials
      "us-east-1"
      (UTCTime (fromGregorian 2013 5 24) 0)
      86400
      SigV4.Request
        { SigV4.method = "GET"
        , SigV4.host = "examplebucket.s3.amazonaws.com"
        , SigV4.path = ["test.txt"]
        , SigV4.headers = []
        }
      `shouldBe` "https://examplebucket.s3.amazonaws.com/test.txt\
                 \?X-Amz-Algorithm=AWS4-HMAC-SHA256\
                 \&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20130524%2Fus-east-1%2Fs3%2Faws4_request\
                 \&X-Amz-Date=20130524T000000Z\
                 \&X-Amz-Expires=86400\
                 \&X-Amz-SignedHeaders=host\
                 \&X-Amz-Signature=aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404"

  it "signs the length and type of an R2 upload, header names in any case" $
    SigV4.presignUrl
      exampleCredentials
      "auto"
      t0
      300
      SigV4.Request
        { SigV4.method = "PUT"
        , SigV4.host = "example.r2.cloudflarestorage.com"
        , SigV4.path = ["unosteve-dev", "00000000-0000-0000-0000-000000000000"]
        , SigV4.headers = [("Content-Length", "20"), ("Content-Type", "text/plain")]
        }
      `shouldBe` "https://example.r2.cloudflarestorage.com/unosteve-dev/00000000-0000-0000-0000-000000000000\
                 \?X-Amz-Algorithm=AWS4-HMAC-SHA256\
                 \&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20260101%2Fauto%2Fs3%2Faws4_request\
                 \&X-Amz-Date=20260101T000000Z\
                 \&X-Amz-Expires=300\
                 \&X-Amz-SignedHeaders=content-length%3Bcontent-type%3Bhost\
                 \&X-Amz-Signature=0a32aef1ec50bf6c051f33a8ba9d1646d957cc39b584985f24b15bd94e11ce9c"
