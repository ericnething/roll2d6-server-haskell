{-# LANGUAGE OverloadedStrings #-}

module Auth
  ( checkAuth
  , checkAuthWai
  , createSession
  )
where

import Data.Monoid (mconcat, (<>))
import Control.Monad (when, join)
import Control.Monad.IO.Class (liftIO)

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types
  ( unauthorized401
  , Status
  , status200
  , status400
  , status500
  )
import Web.Scotty

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Builder (word8Hex, toLazyByteString)

import           Database.PostgreSQL.Simple (Connection)
import qualified Database.Redis as Redis
import           System.Entropy (getEntropy)

import Database (verifyGameAccess)
import Types
  ( PersonId(..)
  , GameId(..)
  , AccessLevel
  )

expiration :: Integer
expiration = minutes 20

hours :: Integer -> Integer
hours h = 3600 * h

minutes :: Integer -> Integer
minutes m = 60 * m

oneYear :: Integer
oneYear = 365 * (hours 24)

checkAuth :: Redis.Connection
          -> (PersonId -> ActionM ())
          -> ActionM ()
checkAuth redisConn handler = do
  let notAuthorized = status unauthorized401 >> text ""
  req <- request
  case getSessionId req of
    Nothing        -> notAuthorized
    Just sessionId -> do
      result <- liftIO $ authenticate redisConn sessionId
      case result of
        Nothing       -> notAuthorized
        Just personId -> handler personId

authenticate :: Redis.Connection
             -> ByteString
             -> IO (Maybe PersonId)
authenticate redisConn sessionId = do
  emPersonId <- liftIO $ Redis.runRedis redisConn $ do
    session  <- Redis.get sessionId
    ettl     <- Redis.ttl sessionId
    let
      extendTTL =
        case ettl of
          Left _ ->
            False
          Right ttl ->
            ttl > 0 && ttl < expiration `div` 2
    when extendTTL $
      Redis.expire sessionId expiration >>
      pure ()
    pure session
  pure $
    case emPersonId of
      Right (Just personId) ->
        Just $ read (BS8.unpack personId)
      _ ->
        Nothing

setAuthCookie :: ByteString -> Integer -> ActionM ()
setAuthCookie sessionId ttl = do
  setHeader "Set-Cookie" (newAuthCookie sessionId ttl)

newAuthCookie :: ByteString -> Integer -> LT.Text
newAuthCookie sessionId ttl =
  LT.intercalate "; "
  [ "session=" <> (LT.fromStrict . T.decodeUtf8 $ sessionId)
  , "HttpOnly"
    -- , "Secure"
  , "Max-Age=" <> (LT.pack . show $ ttl)
  , "Path=/"
  ]

parseCookies :: ByteString -> [(ByteString, ByteString)]
parseCookies =
  map pairs . entries
  where
    entries = splitOn "; "
    pairs =
      fmap (BS.drop 1)
      . BS.break (== (fromIntegral . fromEnum $ '='))

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn sep s =
  case split sep s of
    (a, "") -> a : []
    (a,  b) -> a : splitOn sep b
  where
    split sep s =
      fmap
      (BS.drop $ BS.length sep)
      (BS.breakSubstring sep s)

genSessionId :: IO ByteString
genSessionId = do
  randBytes <- getEntropy 32
  return $ "session:" <> prettyPrint randBytes
  where
    prettyPrint :: ByteString -> ByteString
    prettyPrint
      = LBS.toStrict
      . toLazyByteString
      . mconcat
      . BS.foldr'
      ( \ byte acc -> word8Hex byte:acc ) []

checkAuthWai :: Redis.Connection
             -> Connection
             -> GameId
             -> Request
             -> IO (Maybe AccessLevel)
checkAuthWai redisConn conn gameId req = do
  case getSessionId req of
    Nothing -> pure Nothing
    Just sessionId -> do
      result <- authenticate redisConn sessionId
      case result of
        Nothing -> pure Nothing
        Just personId ->
          verifyGameAccess conn personId gameId

getSessionId :: Request -> Maybe ByteString
getSessionId req =
  maybe Nothing (lookup "session")
  $ parseCookies
  <$> lookup "Cookie" (requestHeaders req)


createSession :: Redis.Connection -> PersonId -> ActionM ()
createSession redisConn personId = do
  sessionId <- liftIO $ genSessionId
  liftIO $ Redis.runRedis redisConn $ do
    Redis.set sessionId (BS8.pack . show $ personId)
    Redis.expire sessionId expiration
  setAuthCookie sessionId oneYear
