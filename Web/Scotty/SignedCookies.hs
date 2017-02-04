{-# LANGUAGE OverloadedStrings #-}
-- | Most of this code depends on OverloadedStrings.
--
-- This is a utility package for Scotty web framework which provides signed cookie functionality
module Web.Scotty.SignedCookies ( getCookie
                                , setCookie ) where

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

data Cookie = Cookie Text Text deriving (Show)

-- | extract the key from the cookie data type
--
cookieK :: Cookie -> Text
cookieK (Cookie k _) = k

-- | extract the value from the cookie data type
--
cookieV :: Cookie -> Text
cookieV (Cookie _ v) = v

-- | parser to extract a cookie
--
parseCookie' :: Parser (Cookie, Text)
parseCookie' = do
  skipSpace
  k <- many1 (letter <|> digit)
  char '='
  v <- many1 (letter <|> digit)
  char '|'
  h <- many1 (letter <|> digit)
  let cookie = Cookie (pack k) (pack v)
  return (cookie, pack h)

-- | parser to extract cookies ending with semo-colon and space
--
parseCookie :: Parser (Cookie, Text)
parseCookie = do
  cook <- parseCookie'
  char ';'
  char ' '
  return cook

-- | primary parse to extract many cookies
--
parseCookies :: Parser [(Cookie, Text)]
parseCookies = many1 $ parseCookie <|> parseCookie'

validateCookie :: Text -> (Cookie, Text) -> Bool
validateCookie s (Cookie k v, h) = h == generateHash (encodeUtf8 s) (encodeUtf8 k <> encodeUtf8 v)

-- | set a cooke
-- > setCookie "secret" "userid" "10"
setCookie :: Text -- ^ secret key to hash values with
          -> Text -- ^ key to store cookie value in
          -> Text -- ^ cookie value
          -> ActionM ()
setCookie s n v = do
  let hash = generateHash (encodeUtf8 s) (encodeUtf8 n <> encodeUtf8 v)
  addHeader "Set-Cookie" $ n <> "=" <> v <> "|" <> hash

-- | geta cookie value if it exists, return Nohting if key doesn't exist or hash value doesn't match
-- > getCookie "secret" "userid"
getCookie :: Text -- ^ secret key to verify hashed values
          -> Text -- ^ key to retrieve
          -> ActionM (Maybe Text)
getCookie secret key = do
  -- get headers as Maybe Text
  h <- header "Cookie"
  -- parse Text of maybe with attoparsec
  case fmap (parseOnly parseCookies . toStrict) h of
    Just a -> case a of
                Right cookies -> if null fcook
                                 then return Nothing
                                 else return $ if validateCookie secret (head fcook)
                                               then Just $ cookieV (fst (head fcook))
                                               else Nothing
                                 where fcook = filter (\(Cookie k _, _) -> k == key) cookies
                _ -> return Nothing
    _ -> return Nothing

generateHash :: ByteString -> ByteString -> Text
generateHash k m = pack . showDigest $ hmacSha256 k m

