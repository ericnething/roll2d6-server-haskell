-- Roll2d6 Virtual Tabletop Project
--
-- Copyright (C) 2018-2019 Eric Nething <eric@roll2d6.org>
--
-- This program is free software: you can redistribute it
-- and/or modify it under the terms of the GNU Affero
-- General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR A
-- PARTICULAR PURPOSE.  See the GNU Affero General Public
-- License for more details.
--
-- You should have received a copy of the GNU Affero General
-- Public License along with this program. If not, see
-- <https://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Monoid (mconcat, (<>))
import Data.Foldable (traverse_)
import Control.Exception (catch, SomeException(..), try, finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
  ( TChan
  , newBroadcastTChanIO
  , writeTChan
  )

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import EventSource (ServerEvent(..), eventSourceAppTChan)
import Network.HTTP.Types
  ( Status
  , status200
  , created201
  , noContent204
  , status400
  , unauthorized401
  , forbidden403
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

import qualified Data.Time as Time
import           Data.Time (UTCTime)
import Web.Scotty

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.ByteString as BS
import           Data.ByteString as BS (ByteString)
import           Data.Binary.Builder (fromByteString)
import qualified Data.UUID.Types as UUID
import Data.Aeson (ToJSON(toEncodingList, toEncoding))
import Data.Aeson.Encoding (fromEncoding)

import           Database.PostgreSQL.Simple (Connection)
import qualified Database.Redis as Redis

import qualified StmContainers.Map as STMMap
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Focus
import Data.Int (Int64)
import qualified ListT

import Auth
import Database
import qualified CouchDB
import Types

main :: IO ()
main =
  -- Create a user presence Map
  STMMap.newIO >>= \presenceMap ->
  
  -- Create a broadcast channel
  newBroadcastTChanIO >>= \chan ->

  -- Ping all subscribers to the channel every 15 seconds
  (forkIO $ pingChannel chan 15) >>

  -- Timeout expired presence
  (forkIO $ timeoutPresence chan presenceMap 60) >>

  -- Connect to Postgres
  getConnection >>= \conn ->

  -- Connect to Redis
  Redis.checkedConnect Redis.defaultConnectInfo >>= \redisConn ->

  -- Create a Manager for the reverse proxy
  newManager defaultManagerSettings >>= \manager ->

  -- Run Scotty
  scotty 3000 $ do
  
  middleware logStdoutDev

  waiApp $ couchProxy manager conn redisConn
  waiApp $ subscription conn redisConn chan presenceMap


  -- API: register a user account
  post "/register" $ do
    reg :: Registration <- jsonData
    mPersonId <- liftIO $ createPerson conn reg
    case mPersonId of
      Nothing ->
        raise "Error: Email already in use."
      Just personId ->
        json personId

  -- API: log in to a user account
  post "/login" $ do
    authData :: AuthenticationData <- jsonData
    mPersonId <- liftIO $ verifyAuthentication conn authData
    case mPersonId of
      Nothing ->
        raise "Error: Failed to login."
      Just personId -> do
        createSession redisConn personId
        json personId

  -- API: log out of a user account
  post "/logout" $ do
    mSessionId <- getSessionId <$> request
    case mSessionId of
      Nothing -> do
        status notFound404
      Just sessionId -> do
        deleteSession redisConn sessionId
        status status200

  -- API: get a list of all games for a user account
  get "/games" $ do
    checkAuth redisConn $ \personId -> do
      games <- liftIO $ getGamesForPersonId conn personId
      json games

  -- API: create a new game
  post "/games" $ do
    checkAuth redisConn $ \personId -> do
      newGame <- jsonData
      rawGameData <- body

      -- create a game entry in postgres
      gameId <- liftIO $
        createGameForPersonId conn
        personId
        newGame

      -- create database in couchdb
      mCouchStatus <- liftIO $
        CouchDB.createDatabase gameId rawGameData
      case mCouchStatus of
        Nothing ->
          status status500
        Just _ -> do
          status created201
          json gameId

  -- API: get a list of all players in a game
  get "/games/:gameId/players" $
    checkAuth redisConn $ \personId -> do
    gameId <- param "gameId"
    mAccess <- liftIO $ verifyGameAccess conn personId gameId
    case mAccess of
      Nothing ->
        status unauthorized401
      Just _ -> do
        players <- liftIO $
                   getUpdatedPlayerList conn gameId presenceMap
        json players


  -- API: add a player to a game
  post "/games/:gameId/players" $
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      playerId <- jsonData
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status unauthorized401
        Just _ -> do
          mResult <- liftIO $ addPersonToGame conn playerId gameId
          case mResult of
            Nothing ->
              status status500 >>
              text "Could not add player to the game"
            Just _ -> do
              players <- liftIO $
                   getUpdatedPlayerList conn gameId presenceMap
              liftIO . atomically $
                writeTChan chan ( BS8.pack . show $ gameId
                                , sse_playerList players)
              status status200

  -- API: create an invite to a game
  put "/games/:gameId/invite" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status unauthorized401
        Just _ -> do
          mInviteId <- createInvite redisConn gameId
          case mInviteId of
            Nothing ->
              status status500
            Just inviteId ->
              json (BS8.unpack $ inviteId)

  -- API: use an invite to join a game
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
                Just _ -> do
                  players <- liftIO $
                    getUpdatedPlayerList conn gameId presenceMap
                  liftIO . atomically $
                    writeTChan chan ( BS8.pack . show $ gameId
                                    , sse_playerList players)
                  json gameId
        _ ->
          status notFound404

  -- API: remove a player from a game
  delete "/games/:gameId/players/:playerId" $
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      playerId <- param "playerId"
      let handler = do
            mResult <- liftIO $
              removePersonFromGame conn playerId gameId
            case mResult of
              Nothing ->
                status status500 >>
                text "Could not remove player from the game"
              Just _ -> do
                players <- liftIO $
                   getUpdatedPlayerList conn gameId presenceMap
                liftIO . atomically $
                  writeTChan chan ( BS8.pack . show $ gameId
                                  , sse_playerList players)
                status status200

      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          -- not an authorized account
          status unauthorized401
        Just Player ->
          -- permission denied
          status forbidden403
        Just _ -> do
          mPlayerAccess <- liftIO $
            verifyGameAccess conn playerId gameId
          case mPlayerAccess of
            Nothing ->
              -- player not found
              status notFound404
            Just Owner ->
              status forbidden403 >>
              text "Game owner cannot be removed from game."
            Just _ ->
              handler


  -- API: ping the server
  post "/games/:gameId/ping" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status forbidden403
        Just _ -> do
          liftIO $
            resetPresenceTimeout chan presenceMap gameId personId
          status noContent204


  -- API: update presence
  post "/games/:gameId/presence/:presence" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      presence :: T.Text <- param "presence"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status forbidden403
        Just _ -> do
          case presence of
            "online" -> do
              liftIO $
                setPlayerOnline chan presenceMap gameId personId
              status noContent204
            "offline" -> do
              liftIO $
                setPlayerOffline chan presenceMap gameId personId
              status noContent204
            _ ->
              status notFound404

  -- API: insert a new chat message
  post "/games/:gameId/chat" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status forbidden403
        Just _ -> do
          newMessage <- jsonData
          result <- liftIO $
            insertChatMessage conn personId gameId newMessage
          case result of
            Nothing ->
              status status500
            Just chatMessage -> do
              status created201
              liftIO . atomically $
                writeTChan chan ( BS8.pack . show $ gameId
                                , sse_chatMessage [chatMessage])


  -- API: get a chat log
  get "/games/:gameId/chat" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status forbidden403
        Just _ -> do
          chatLog <- liftIO $ getChatLog conn gameId 50
          json chatLog


  -- API: get my player id
  get "/games/:gameId/player-id" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          status forbidden403
        Just _ ->
          json personId


  -- API: generate a new game sheet id
  post "/games/:gameId/new-sheet-id" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      mAccess <- liftIO $ verifyGameAccess conn personId gameId
      case mAccess of
        Nothing ->
          -- not an authorized account
          status unauthorized401
        Just Player ->
          -- permission denied
          status forbidden403
        Just _ -> do
          muuid <- liftIO $ generateNewSheetUUID conn gameId
          case muuid of
            Nothing ->
              status status500
            Just uuid -> do
              status created201
              json $ UUID.toText uuid


  -- API: delete a game sheet id
  delete "/games/:gameId/sheet-id/:sheetId" $ do
    checkAuth redisConn $ \personId -> do
      gameId <- param "gameId"
      msheetId <- UUID.fromText <$> param "sheetId"
      case msheetId of
        Nothing -> do
          status status400
          text "Not a valid sheetId"
        Just sheetId -> do
          mAccess <- liftIO $ verifyGameAccess conn personId gameId
          case mAccess of
            Nothing ->
              -- not an authorized account
              status unauthorized401
            Just Player ->
              -- permission denied
              status forbidden403
            Just _ -> do
              result <- liftIO $ deleteSheetUUID conn gameId sheetId
              case result of
                Nothing ->
                  status status500
                Just uuid ->
                  json $ UUID.toText uuid

------------------------------------------------------------
-- Raw Endpoints
------------------------------------------------------------

waiApp :: Middleware -> ScottyM ()
waiApp = middleware

pingSubscriber :: TChan (ByteString, ServerEvent) -> IO ()
pingSubscriber chan =
  atomically $ writeTChan chan ("*", CommentEvent "")

subscription :: Connection
             -> Redis.Connection
             -> TChan (ByteString, ServerEvent)
             -> STMMap.Map ByteString (Map PersonId UTCTime)
             -> Application
             -> Application
subscription conn redisConn chan presenceMap app req resp =
  case (requestMethod req, pathInfo req) of
    (_, "subscribe":rawGameId:_) -> do
      let
        toGameId =
          fmap GameId
          . UUID.fromText
          . T.drop 1
          . T.dropWhile (/= '_')
      case toGameId rawGameId of
        Nothing ->
          resp $ responseLBS notFound404 [] ""
        Just gameId -> do
          result <- checkAuthWai redisConn conn gameId req
          case result of
            Nothing ->
              resp $ responseLBS unauthorized401 [] ""
            Just (personId, access) -> do
              setPlayerOnline chan presenceMap gameId personId
              eventSourceAppTChan
                (BS8.pack . show $ gameId) chan req resp
              resp $ responseLBS noContent204 [] ""

    _ -> app req resp


setPlayerOnline :: TChan (ByteString, ServerEvent)
                -> STMMap.Map ByteString (Map PersonId UTCTime)
                -> GameId
                -> PersonId
                -> IO ()
setPlayerOnline chan presenceMap gameId personId = do
      putStrLn $ show personId <> " is online"
      now <- Time.getCurrentTime
      atomically $ STMMap.focus
        (Focus.insertOrMerge
          Map.union
          (Map.singleton personId now))
        (BS8.pack . show $ gameId)
        presenceMap
      atomically $
        writeTChan
        chan
        ( BS8.pack . show $ gameId
        , sse_playerPresence [PersonPresence personId True])

setPlayerOffline :: TChan (ByteString, ServerEvent)
                 -> STMMap.Map ByteString (Map PersonId UTCTime)
                 -> GameId
                 -> PersonId
                 -> IO ()
setPlayerOffline chan presenceMap gameId personId = do
      putStrLn $ show personId <> " is offline"
      atomically $ STMMap.focus
        (Focus.update (Just . Map.delete personId))
        (BS8.pack . show $ gameId)
        presenceMap
      atomically $
        writeTChan
        chan
        ( BS8.pack . show $ gameId
        , sse_playerPresence [PersonPresence personId False])

resetPresenceTimeout :: TChan (ByteString, ServerEvent)
                     -> STMMap.Map ByteString (Map PersonId UTCTime)
                     -> GameId
                     -> PersonId
                     -> IO ()
resetPresenceTimeout chan presenceMap gameId personId = do
      now <- Time.getCurrentTime
      atomically $ STMMap.focus
        (Focus.insertOrMerge
          Map.union (Map.singleton personId now))
        (BS8.pack . show $ gameId)
        presenceMap

removeExpiredPresenceForGame :: TChan (ByteString, ServerEvent)
                             -> STMMap.Map ByteString (Map PersonId UTCTime)
                             -> ByteString
                             -> IO ()
removeExpiredPresenceForGame chan presenceMap gameId = do
  let gameKey = gameId -- BS8.pack . show $ gameId
  now <- Time.getCurrentTime
  expired <- atomically $ do
    mGame <- STMMap.focus Focus.lookup gameKey presenceMap
    case mGame of
      Nothing -> pure Map.empty
      Just game -> do
        let (expired, valid) = Map.partition (isExpired now) game
        STMMap.focus
          (Focus.update (const $ Just valid))
          gameKey
          presenceMap
        pure expired
  atomically $ writeTChan chan
    (gameKey, sse_playerPresence (toPresenceList expired))
  where
    isExpired now = (> 60) . Time.diffUTCTime now
    toPresenceList expired =
      map
      (\personId -> PersonPresence personId False)
      (Map.keys expired)
      
removeExpiredPresence :: TChan (ByteString, ServerEvent)
                      -> STMMap.Map ByteString (Map PersonId UTCTime)
                      -> IO ()
removeExpiredPresence chan presenceMap = do
  keys <- atomically . (fmap . fmap) fst $
          ListT.toList (STMMap.listT presenceMap)
  traverse_ (removeExpiredPresenceForGame chan presenceMap) keys


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
          resp $ responseLBS notFound404 [] ""
        Just gameId -> do
          result <- checkAuthWai redisConn conn gameId req
          case result of
            Nothing ->
              resp $ responseLBS unauthorized401 [] ""
            Just (_, access) ->
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




sse_playerList :: [Person] -> ServerEvent
sse_playerList players =
  ServerEvent
  { eventName = Just (fromByteString "player-list")
  , eventId = Nothing
  , eventData = [fromEncoding (toEncodingList players)]
  }

sse_playerPresence :: [PersonPresence] -> ServerEvent
sse_playerPresence presenceList =
  ServerEvent
  { eventName = Just (fromByteString "player-presence")
  , eventId = Nothing
  , eventData = [fromEncoding (toEncodingList presenceList)]
  }

sse_chatMessage :: [ChatMessage] -> ServerEvent
sse_chatMessage chatMessages =
  ServerEvent
  { eventName = Just (fromByteString "chat-message")
  , eventId = Nothing
  , eventData = [fromEncoding (toEncodingList chatMessages)]
  }

pingChannel :: TChan (ByteString, ServerEvent) -> Int -> IO ()
pingChannel chan seconds =
  forever $
  threadDelay (seconds * 1000 * 1000)
  >> pingSubscriber chan


timeoutPresence :: TChan (ByteString, ServerEvent)
                -> STMMap.Map ByteString (Map PersonId UTCTime)
                -> Int
                -> IO ()
timeoutPresence chan presenceMap seconds =
  forever $
  threadDelay (seconds * 1000 * 1000)
  >> removeExpiredPresence chan presenceMap


getUpdatedPlayerList :: Connection
                     -> GameId
                     -> STMMap.Map ByteString (Map PersonId UTCTime)
                     -> IO [Person]
getUpdatedPlayerList conn gameId presenceMap = do
  players <- listPeopleInGame conn gameId
  mPresence <- atomically $ STMMap.focus
    Focus.lookup
    (BS8.pack . show $ gameId)
    presenceMap
  case mPresence of
    Nothing ->
      pure players
    Just presence ->
      pure $ map (updatePresence presence) players
