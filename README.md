
# Signed Cookies for Scotty Web Framework

The signed-cookies package exports two functions `setCookie` and `getCookie`.

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
  h <- getCookie "secret" "key"
  text $ pack . show $ h

main :: IO ()
main = scotty 3000 $ do
  get "/" singleCookie
  get "/read" readCookies
```

Also, here is a helper function if you need to access the cookie via Javascript

```
function get_cookie(name) {
    const cookies = document.cookie.split(';');
    var retCookies = {};
    for (var i in cookies) {
        const parts = cookies[i].split('=');
        if (parts.length == 2) {
            const parts2 = parts[1].split('|');
            if (parts2.length == 2) {
                retCookies[parts[0].trim()] = parts2[0].trim();
            }
        }
    }
    return (name in retCookies) ? retCookies[name] : null;
}
```
