{-# LANGUAGE OverloadedStrings #-}
-- | Most of this code depends on OverloadedStrings.
--
-- This is a utility package for Scotty web framework which provides signed cookie functionality
module Web.Scotty.SignedCookies ( setSignedCookie
                                , getSignedCookie
                                , deleteCookie
                                ) where

import Control.Monad.IO.Class
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.Digest.Pure.SHA
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding as E
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as S
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Web.Scotty
import Web.Scotty.Internal.Types (ActionError (Next))
import Data.Attoparsec.Text
import Control.Applicative
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | parser to extract a cookie
-- --
parseCookie' :: Parser (SetCookie, Text)
parseCookie' = do
  skipSpace
  k <- many1 (letter <|> digit)
  char '='
  v <- many1 (letter <|> digit)
  char '|'
  h <- many1 (letter <|> digit)
  let cookie = def { setCookieName = S.pack k 
                   , setCookieValue = S.pack v }
  return (cookie, pack h)
  
-- | parser to extract cookies ending with semo-colon and space
--
parseCookie :: Parser (SetCookie, Text)
parseCookie = do
  cook <- parseCookie'
  char ';'
  char ' '
  return cook

-- | primary parse to extract many cookies
--
getCookies :: Parser [(SetCookie, Text)]
getCookies = many1 $ parseCookie <|> parseCookie'

setSignedCookie :: Text -- ^ secret
                -> SetCookie -- ^ cookie
                -> ActionM ()
setSignedCookie secret cookie = do
  let cv = LBS.fromStrict $ setCookieName cookie <> setCookieValue cookie
      bs = (LTE.encodeUtf8 . LT.fromStrict) secret
      hash = S.pack $ generateHash bs cv
      newCookie = def { setCookieName = setCookieName cookie
                      , setCookieValue = setCookieValue cookie <> "|" <> hash
                      , setCookiePath = setCookiePath cookie
                      , setCookieExpires = setCookieExpires cookie
                      , setCookieMaxAge = setCookieMaxAge cookie
                      , setCookieDomain = setCookieDomain cookie
                      , setCookieHttpOnly = setCookieHttpOnly cookie
                      , setCookieSecure = setCookieSecure cookie
                      , setCookieSameSite = setCookieSameSite cookie }
  addHeader "Set-Cookie" $ (LTE.decodeUtf8 . toLazyByteString . renderSetCookie) newCookie

setCookieToText = LTE.decodeUtf8 . toLazyByteString . renderSetCookie

-- | geta cookie value if it exists, return Nohting if key doesn't exist or hash value doesn't match
-- > getSignedCookie "secret" "userid"
getSignedCookie :: Text -- ^ secret key to verify hashed values
                -> Text -- ^ key to retrieve
                -> ActionM (Maybe Text)
getSignedCookie secret key = do
  -- get headers as Maybe Text
  h <- header "Cookie"
  -- parse Text of maybe with attoparsec
  let maybeCookies = fmap (parseOnly getCookies . LT.toStrict) h 
  case maybeCookies of
    Just a -> case a of
                Right cookies -> if null filteredCookies
                                 then return Nothing
                                 else return response
                                 where filteredCookies = filter (\(c, _) -> E.decodeUtf8 (setCookieName c) == key) cookies 
                                       filteredAndVerified = filter (validateCookie secret) filteredCookies 
                                       response = if null filteredAndVerified 
                                                  then Nothing 
                                                  else Just $ (E.decodeUtf8 . setCookieValue) $ fst (head filteredAndVerified)
                _             -> return Nothing
    _      -> return Nothing

deleteCookie :: Text -- ^ key to remove
             -> ActionM ()
deleteCookie key = do
  let cookie = def { setCookieName = E.encodeUtf8 key
                   , setCookieValue = ""
                   , setCookieExpires = Just (posixSecondsToUTCTime 1) }
  addHeader "Set-Cookie" $ setCookieToText cookie

validateCookie :: Text -- ^ secret
               -> (SetCookie, Text) -- ^ cookie and hashed parsed from request headers
               -> Bool
validateCookie s (c, h) = do
  let bs = (LTE.encodeUtf8 . LT.fromStrict) s
  h == pack (generateHash bs (LBS.fromStrict (setCookieName c <> setCookieValue c)))

generateHash :: LBS.ByteString -- ^ secret
             -> LBS.ByteString -- ^ concat [cookeName, cookieValue]
             -> String 
generateHash secret cookie = showDigest $ hmacSha256 secret cookie


