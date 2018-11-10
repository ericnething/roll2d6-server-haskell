{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid (mconcat, (<>))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan)

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Network.HTTP.Types
  ( Status
  , status200
  , status400
  , unauthorized401
  , notFound404
  , status500
  )
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

import Web.Scotty

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString as BS
import           Data.ByteString as BS (ByteString)
import           Data.Binary.Builder (fromByteString)
import qualified Data.UUID.Types as UUID

import           Database.PostgreSQL.Simple (Connection)
import qualified Database.Redis as Redis

import Auth
import Database
import qualified CouchDB
import Types
  ( Registration
  , AuthenticationData
  , PersonId(..)
  , GameId(..)
  , NewGame(..)
  , AccessLevel(..)
  )

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
        createSession redisConn personId
        json personId


  post "/logout" $ do
    mSessionId <- getSessionId <$> request
    case mSessionId of
      Nothing -> do
        status notFound404
        text ""
      Just sessionId -> do
        deleteSession redisConn sessionId
        status status200
        text ""


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


  put "/games/:gameId/invite" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status unauthorized401 >> text ""
        Just _ -> do
          mInviteId <- createInvite redisConn gameId
          case mInviteId of
            Nothing ->
              status status500 >> text ""
            Just inviteId ->
              json (BS8.unpack $ inviteId)


  post "/invite/:inviteId" $
    checkAuth redisConn $ \personId -> do
      inviteId <- param "inviteId"
      emGameId <- liftIO $ Redis.runRedis redisConn $ do
        Redis.get ("invite:" <> inviteId)
      case emGameId of
        Right (Just rawGameId) ->
          case GameId <$> UUID.fromASCIIBytes rawGameId of
            Nothing ->
              status status500 >>
              text "The game id is not valid"
            Just gameId -> do
              mResult <- liftIO $
                addPersonToGame conn personId gameId
              case mResult of
                Nothing ->
                  status status500 >>
                  text "Could not add player to the game"
                Just _ ->
                  json gameId
        _ ->
          status notFound404 >> text ""


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
    (_, "couchdb":"":[]) -> do
      waiProxyTo handler defaultOnExc manager req resp

    (_, "couchdb":rawGameId:_) -> do
      let
        toGameId =
          fmap GameId
          . UUID.fromText
          . T.drop 1
          . T.dropWhile (/= '_')

      case toGameId rawGameId of
        Nothing ->
          resp $ responseLBS unauthorized401 [] ""
        Just gameId -> do
          result <- checkAuthWai redisConn conn gameId req
          case result of
            Nothing ->
              resp $ responseLBS unauthorized401 [] ""
            Just access ->
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
      pure $ WPRModifiedRequest newReq $ ProxyDest "127.0.0.1" 5984
