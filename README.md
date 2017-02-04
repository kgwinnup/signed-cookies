
```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionError(Next))
import Data.Digest.Pure.SHA
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (fromStrict, Text, pack, toStrict)
import Data.Monoid ((<>))
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator (manyTill, lookAhead)
import Control.Applicative
import Control.Monad.IO.Class

import Web.Scotty.SecureCookies

singleCookie = do
  setCookie "secret" "key" "value"
  setCookie "secret" "key2" "value2"
  text "single cookie"

readCookies = do
  cs <- header "Cookie"
  h <- getCookie "secret" "key"
  text $ pack . show $ h

main :: IO ()
main = scotty 3000 $ do
  get "/" singleCookie
  get "/read" readCookies
```
