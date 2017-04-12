{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.Hspec
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE

import Web.Scotty.SignedCookies.SignedCookiesInternal

stbs = LTE.encodeUtf8 . LT.fromStrict 

secret = stbs "secret"
cookieId = stbs "id"
cookieValue = stbs "valuehere"

main :: IO ()
main = hspec $ do
  describe "Hash validation" $ do
    it "generates a hash from a bytestring" $ do
      length (generateHash cookieId cookieValue) `shouldBe` 64
      generateHash secret (cookieId <> cookieValue) `shouldBe` "895673090f6d4d7ae837bee687e23205f6c267a4de4e87df2bcc9415f02771cc"
