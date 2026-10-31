module UnoSteve.StorageSpec (spec) where

import Test.Hspec

import Test.Support (testName)
import UnoSteve.Storage (contentDisposition)

spec :: Spec
spec = describe "contentDisposition" $ do
  it "repeats a plain ASCII name in both forms" $
    contentDisposition (testName "report.pdf")
      `shouldBe` "attachment; filename=\"report.pdf\"; filename*=UTF-8''report.pdf"

  it "percent-encodes UTF-8 and replaces non-ASCII in the fallback" $
    contentDisposition (testName "résumé 2026.pdf")
      `shouldBe` "attachment; filename=\"r_sum_ 2026.pdf\"; filename*=UTF-8''r%C3%A9sum%C3%A9%202026.pdf"

  it "can't be broken by quotes or apostrophes" $
    contentDisposition (testName "say \"hi\" 'now'.txt")
      `shouldBe` "attachment; filename=\"say _hi_ 'now'.txt\"; filename*=UTF-8''say%20%22hi%22%20%27now%27.txt"
