{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat, (<>))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.EventSource
  ( ServerEvent(..)
  , eventSourceAppChan
  )
import Network.HTTP.Types
  ( unauthorized401
  , Status
  , status200
  , status400
  , status500
  )
import Types
  ( Registration
  , AuthenticationData
  , PersonId(..)
  , GameId(..)
  , NewGame(..)
  )
import Network.Wai
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Data.Binary.Builder (fromByteString)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import Data.ByteString.Char8 as BS8 (pack, unpack)
import Data.ByteString as BS (ByteString, foldr', dropWhile, drop)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Data.ByteString.Builder (word8Hex, toLazyByteString)

import Data.Int (Int64)

import Database
import Database.PostgreSQL.Simple (Connection)
import qualified Database.Redis as Redis
import System.Entropy (getEntropy)
import qualified CouchDB
import Network.HTTP.Client
  ( Manager
  , defaultManagerSettings
  , newManager
  )
import Network.HTTP.ReverseProxy
  ( waiProxyTo
  , WaiProxyResponse(..)
  , defaultOnExc
  , ProxyDest(..)
  )
import qualified Data.UUID.Types as UUID (fromText)


main :: IO ()
main =
  newChan >>= \chan ->
  getConnection >>= \conn ->
  Redis.checkedConnect Redis.defaultConnectInfo >>= \redisConn ->
  newManager defaultManagerSettings >>= \manager ->
  scotty 3000 $ do
  
  middleware logStdoutDev

  waiApp $ couchProxy manager conn redisConn
  waiApp $ subscription chan


  get "/" $ do
    setHeader "Content-Type" "text/html"
    file "src/event-source.html"


  post "/send" $ do
    msg <- param "msg"
    let event =
          ServerEvent
          { eventName = Just (fromByteString "Message")
          , eventId = Nothing
          , eventData = [fromByteString (T.encodeUtf8 msg)]
          }
    liftIO $ writeChan chan event


  post "/register" $ do
    reg :: Registration <- jsonData
    mPersonId <- liftIO $ createPerson conn reg
    case mPersonId of
      Nothing ->
        raise "Error: Email already in use."
      Just personId ->
        json personId


  post "/login" $ do
    authData :: AuthenticationData <- jsonData
    mPersonId <- liftIO $ verifyAuthentication conn authData
    case mPersonId of
      Nothing ->
        raise "Error: Failed to login."
      Just personId -> do
        sessionId <- liftIO $ genSessionId
        let expiration = minutes 20
        liftIO $ Redis.runRedis redisConn $ do
          Redis.set sessionId (BS8.pack . show $ personId)
          Redis.expire sessionId expiration
        setAuthCookie sessionId expiration
        json personId

  get "/games" $ do
    checkAuth redisConn $ \personId -> do
      games <- liftIO $ getGamesForPersonId conn personId
      json games


  post "/games" $ do
    checkAuth redisConn $ \personId -> do
      newGame <- jsonData

      -- create a game entry in postgres
      gameId <- liftIO $
        createGameForPersonId conn
        personId
        (_newGameTitle newGame)

      -- create database in couchdb
      mCouchStatus <- liftIO $
        CouchDB.createDatabase gameId
      case mCouchStatus of
        Nothing ->
          status status500 >> text ""
        Just _ ->
          json gameId


  get "/games/:gameId/players" $
    checkAuth redisConn $ \personId -> do
    gameId <- param "gameId"
    mAccess <- liftIO $ verifyGameAccess conn personId gameId
    case mAccess of
      Nothing ->
        status unauthorized401 >> text ""
      Just _ -> do
        players <- liftIO $ listPeopleInGame conn gameId
        json players


  post "/games/:gameId/players" $
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      playerId <- jsonData
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status unauthorized401 >> text ""
        Just _ -> do
          mResult <- liftIO $ addPersonToGame conn playerId gameId
          case mResult of
            Nothing ->
              status status500 >>
              text "Could not add player to the game"
            Just _ ->
              status status200 >>
              text ""

  delete "/games/:gameId/players/:playerId" $
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      playerId <- param "playerId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status unauthorized401 >> text ""
        Just _ -> do
          mResult <- liftIO $
            removePersonFromGame conn playerId gameId
          case mResult of
            Nothing ->
              status status500 >>
              text "Could not remove player from the game"
            Just _ ->
              status status200 >>
              text ""


------------------------------------------------------------
-- Authentication/Authorization
------------------------------------------------------------

checkAuth :: Redis.Connection
          -> (PersonId -> ActionM ())
          -> ActionM ()
checkAuth redisConn handler = do
  result <- authenticate redisConn
  case result of
    Left err       -> status err >> text ""
    Right personId -> handler personId

authenticate :: Redis.Connection -> ActionM (Either Status PersonId)
authenticate redisConn = do
  cookies <- getCookies
  case Map.lookup "session" cookies of
      Nothing ->
        pure $ Left unauthorized401
      Just sessionId -> do
        -- lookup session in database to get person id
        emPersonId <- liftIO $ Redis.runRedis redisConn $ do
          Redis.get (T.encodeUtf8 . T.toStrict $ sessionId)
        pure $ case emPersonId of
          Left err -> do
            Left unauthorized401
          Right mPersonId ->
            case mPersonId of
              Nothing ->
                Left unauthorized401
              Just personId ->
                Right $ PersonId $ read (BS8.unpack personId)

            
getCookies :: ActionM (Map Text Text)
getCookies = do
  mRawCookies <- header "Cookie"
  pure $
    case mRawCookies of
      Nothing -> Map.empty
      Just raw -> parseCookies raw

setAuthCookie :: ByteString -> Integer -> ActionM ()
setAuthCookie sessionId ttl = do
  setHeader "Set-Cookie" cookie
  where
    cookie =
      T.intercalate "; "
      [ "session=" <> (T.fromStrict . T.decodeUtf8 $ sessionId)
      , "HttpOnly"
      -- , "Secure"
      , "Max-Age=" <> (T.pack . show $ ttl)
      -- , "Domain=" <> (T.pack .show $ domain)
      -- , "Path=/"
      ]

parseCookies :: Text -> Map Text Text
parseCookies =
  Map.fromList . map pairs . entries
  where
    entries = T.splitOn "; "
    pairs = fmap (T.drop 1) . T.breakOn "="

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

hours :: Integer -> Integer
hours h = 3600 * h

minutes :: Integer -> Integer
minutes m = 60 * m

------------------------------------------------------------
-- Raw Endpoints
------------------------------------------------------------

waiApp :: Middleware -> ScottyM ()
waiApp = middleware

subscription :: Chan ServerEvent -> Application -> Application
subscription chan app req resp =
  case (requestMethod req, pathInfo req) of
    (_, "subscribe":_) ->
      eventSourceAppChan chan req resp
    _ -> app req resp

couchProxy :: Manager
           -> Connection
           -> Redis.Connection
           -> Application -> Application
couchProxy manager conn redisConn app req resp =
  case (requestMethod req, pathInfo req) of
    (_, "couchdb":_) ->
      waiProxyTo handler defaultOnExc manager req resp
    _ -> app req resp
  where
    handler req = do
      let newReq =
            req
            { pathInfo = tail (pathInfo req)
            , rawPathInfo =
                BS.dropWhile (/= (fromIntegral . fromEnum $ '/'))
                . BS.drop 1
                $ rawPathInfo req
            }
      putStrLn $ "rawPathInfo: " <> show (rawPathInfo newReq)
      putStrLn $ "pathInfo: " <> show (pathInfo newReq)
      pure $ WPRModifiedRequest newReq $ ProxyDest "127.0.0.1" 5984
