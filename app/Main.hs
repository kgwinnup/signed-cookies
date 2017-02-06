{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionError(Next))
import Data.Text.Lazy (fromStrict, Text, pack, toStrict)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty.SignedCookies
import Web.Cookie
import Data.Time.Clock.POSIX (getCurrentTime)

singleCookie = do
  cur <- liftIO getCurrentTime
  setSignedCookie "secret" $ def { setCookieName = "id"
                                 , setCookieValue = "valuehere" }
  text "single cookie"

readCookies = do
  h <- getSignedCookie "secret" "id"
  text $ pack . show $ h

removeCookies = do
  deleteCookie "id"
  text "deleted"

main :: IO ()
main = scotty 3000 $ do
  get "/" singleCookie
  get "/read" readCookies
  get "/del" removeCookies
  get "/empty" $ text "hello"

