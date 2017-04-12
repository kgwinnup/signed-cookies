{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.SignedCookies.SignedCookiesInternal where

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



