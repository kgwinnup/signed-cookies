
# Signed Cookies for Scotty Web Framework

The signed-cookies package exports two functions `setCookie` and `getCookie`. Besure to use the same signing key.

## Example

```
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionError(Next))
import Data.Text.Lazy (fromStrict, Text, pack, toStrict)

import Web.Scotty.SignedCookies

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
