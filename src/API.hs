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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module API where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Network.Wai
import Network.HTTP.ReverseProxy
  ( waiProxyTo
  , WaiProxyResponse(..)
  , defaultOnExc
  , ProxyDest(..)
  )
import Network.HTTP.Types
  ( noContent204
  , unauthorized401
  , forbidden403
  , notFound404
  )

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString as BS
import           Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import           Data.Binary.Builder (toLazyByteString)
import qualified Data.UUID.Types as UUID
import           Data.UUID.Types (UUID)

import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth
import Web.Cookie (SetCookie, renderSetCookie)

import Auth
import qualified Database as DB
import Database (runDB, runDBwithConfig)
import qualified CouchDB
import Types
import Config (Config(..))
import qualified Redis
import           Redis (redis)

type instance AuthServerData (AuthProtect "cookie-auth") = PersonId  

type API
  = "api" :>
  (      RegisterAccount
    :<|> LoginAccount
    :<|> GetGames
    :<|> CreateGame
    :<|> AcceptGameInvite
    :<|> CreateGameInvite
    :<|> UpdateGameTitle
    :<|> CreateNewSheetId
    :<|> DeleteSheetId
    :<|> GetPlayers
    :<|> AddPlayer
    :<|> RemovePlayer
    :<|> GetMyPlayerInfo
    :<|> InsertChatMessage
    :<|> GetChatLog
    :<|> LogoutAccount
    :<|> CouchProxy
  )

api :: Proxy API
api = Proxy

nt :: Config -> App a -> Handler a
nt s x = runReaderT x s

app :: Config -> Application
app config = let context = authContext config in
  serveWithContext api context (server config)

authContext :: Config
            -> Context (AuthHandler Request PersonId ': '[])
authContext config = authHandler config :. EmptyContext

authContextProxy :: Proxy '[AuthHandler Request PersonId]
authContextProxy = Proxy

server :: Config -> Server API
server config =
  hoistServerWithContext api authContextProxy (nt config)
  (      registerAccount
    :<|> loginAccount
    :<|> myGameList
    :<|> createNewGame
    :<|> useGameInvite
    :<|> createGameInvite
    :<|> updateGameTitle
    :<|> createNewGameSheetId
    :<|> deleteGameSheetId
    :<|> getPlayers
    :<|> addPlayer
    :<|> removePlayer
    :<|> getMyPlayerInfo
    :<|> newChatMessage
    :<|> getChatLog
    :<|> logoutAccount config
    :<|> couchServer config
  )


type CouchProxy =
  AuthProtect "cookie-auth"
  :> "couchdb" :> Raw

couchServer :: Config -> PersonId -> Tagged App Application
couchServer config personId =
  Tagged (couchProxy config personId)


-- Register a user account
type RegisterAccount =
  "register"
  :> ReqBody '[JSON] Registration
  :> PostCreated '[JSON] PersonId

registerAccount :: Registration -> App PersonId
registerAccount reg = do
    mPersonId <- runDB $ DB.createPerson reg
    case mPersonId of
      Nothing -> throwError err409
      Just personId -> pure personId

-- Log in to a user account
type LoginAccount =
  "login"
  :> ReqBody '[JSON] AuthenticationData
  :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] PersonId)

loginAccount :: AuthenticationData
             -> App (Headers '[Header "Set-Cookie" SetCookie]
                     PersonId)
loginAccount authData = do
    mPersonId <- runDB $ DB.verifyAuthentication authData
    case mPersonId of
      Nothing -> throwError err400
      Just personId -> do
        cookie <- createSession personId
        pure . addHeader cookie $ personId


type LogoutAccount =
  AuthProtect "cookie-auth"
  :> "logout" :> Raw

logoutAccount :: Config -> PersonId -> Tagged App Application
logoutAccount config personId = Tagged $ \request respond -> do
  let mSessionId = getSessionId request
  case mSessionId of
    Nothing -> respond $ responseLBS notFound404 [] ""
    Just sessionId -> do
      cookie <- LBS.toStrict . toLazyByteString . renderSetCookie
                <$> deleteSession config sessionId
      respond $
        responseLBS noContent204 [("Set-Cookie", cookie)] ""


-- Get a list of all your games
type GetGames =
  AuthProtect "cookie-auth"
  :> "games"
  :> Get '[JSON] [Game]

myGameList :: PersonId -> App [Game]
myGameList myId =
  runDB $ DB.getGamesForPersonId myId


-- Create a new game
type CreateGame =
  AuthProtect "cookie-auth"
  :> "games"
  :> ReqBody '[JSON] NewGame
  :> PostCreated '[JSON] GameId

createNewGame :: PersonId -> NewGame -> App GameId
createNewGame myId newGame = do
  -- create a game entry in postgres
  gameId <- runDB $ DB.createGameForPersonId myId newGame

  -- create database in couchdb
  mCouchStatus <- liftIO $
    CouchDB.createDatabase gameId newGame
  case mCouchStatus of
    Nothing ->
      throwError err500
    Just _ -> do
      pure gameId


-- Update game title
type UpdateGameTitle =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "title"
  :> ReqBody '[JSON] Text
  :> PostNoContent '[JSON] NoContent

updateGameTitle :: PersonId -> GameId -> Text -> App NoContent
updateGameTitle myId gameId title = do
  verifyGameAccess myId gameId $ \access -> do
    mResult <- runDB $ DB.updateGameTitle gameId title
    case mResult of
      Nothing -> throwError $
        err500 { errBody = "Could not update game title" }
      Just _ -> pure NoContent


-- Get a list of all players in a game
type GetPlayers =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "players"
  :> Get '[JSON] [Person]

getPlayers :: PersonId -> GameId -> App [Person]
getPlayers myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    runDB $ DB.listPeopleInGame gameId


-- Add a player to a game
type AddPlayer =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "players"
  :> ReqBody '[JSON] PersonId
  :> PostNoContent '[JSON] NoContent

addPlayer :: PersonId -> GameId -> PersonId -> App NoContent
addPlayer myId gameId newPlayerId = do
  verifyGameAccess myId gameId $ \access -> do
    mResult <- runDB $ DB.addPersonToGame newPlayerId gameId
    case mResult of
      Nothing -> throwError $
        err500 { errBody = "Could not add player to the game" }
      Just _ -> do
        players <- runDB $ DB.listPeopleInGame gameId
        -- TODO: Send updated player list message
        pure NoContent


-- Create an invite code for your game
type CreateGameInvite =
  AuthProtect "cookie-auth"
  :> "invite" :> "games"
  :> Capture "gameId" GameId
  :> PutCreated '[JSON] InviteCode

createGameInvite :: PersonId -> GameId -> App InviteCode
createGameInvite myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    createInvite gameId


-- Use an invite to join a game
type AcceptGameInvite =
  AuthProtect "cookie-auth"
  :> "invite"
  :> Capture "inviteCode" InviteCode
  :> Post '[JSON] GameId

useGameInvite :: PersonId -> InviteCode -> App GameId
useGameInvite myId inviteId = do
  emGameId <- redis $ Redis.lookupGameInvite inviteId
  case emGameId of
    Right (Just rawGameId) ->
      case GameId <$> UUID.fromASCIIBytes rawGameId of
        Nothing -> throwError $
          err500 { errBody = "The game id is not valid" }
        Just gameId -> do
          mResult <- runDB $ DB.addPersonToGame myId gameId
          case mResult of
            Nothing -> throwError $
              err500 { errBody = "Could not add player to the game" }
            Just _ -> do
              players <- runDB $ DB.listPeopleInGame gameId
              -- TODO: Send updated player list message
              pure gameId
    _ -> throwError err404


-- Remove a player from a game
type RemovePlayer =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "players"
  :> Capture "playerId" PersonId
  :> Delete '[JSON] NoContent

removePlayer :: PersonId -> GameId -> PersonId -> App NoContent
removePlayer myId gameId removedPlayerId = do
  let handler = do
        mResult <- runDB $
          DB.removePersonFromGame removedPlayerId gameId
        case mResult of
          Nothing -> throwError $
            err500 {errBody = "Could not remove player from the game" }
          Just _ -> do
            players <- runDB $ DB.listPeopleInGame gameId
            -- TODO: Send updated player list message
            pure NoContent

  verifyGameAccess myId gameId $ \access -> do
    case access of
      Player ->
        -- permission denied
        throwError err403
      _ -> do
        mPlayerAccess <- runDB $
          DB.verifyGameAccess removedPlayerId gameId
        case mPlayerAccess of
          Nothing ->
            -- player not found
            throwError err404
          Just Owner -> throwError $
            err403 { errBody = "Game owner cannot be removed from game." }
          Just _ -> handler


-- Insert a new chat message
type InsertChatMessage =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "chat"
  :> ReqBody '[JSON] NewChatMessage
  :> PostCreated '[JSON] NoContent

newChatMessage :: PersonId
               -> GameId
               -> NewChatMessage
               -> App NoContent
newChatMessage myId gameId newMessage = do
  verifyGameAccess myId gameId $ \access -> do
    result <- runDB $ DB.insertChatMessage myId gameId newMessage
    case result of
      Nothing -> throwError err500
      Just chatMessage -> do
        -- TODO: Send out new chat message
        pure NoContent


-- Get a chat log
type GetChatLog =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "chat"
  :> Get '[JSON] [ChatMessage]

getChatLog :: PersonId -> GameId -> App [ChatMessage]
getChatLog myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    runDB $ DB.getChatLog gameId 50


-- Get my player information
type GetMyPlayerInfo =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "my-player-info"
  :> Get '[JSON] Person

getMyPlayerInfo :: PersonId -> GameId -> App Person
getMyPlayerInfo myId gameId = do
  mPlayerInfo <- runDB $ DB.getPlayerInfo myId gameId
  case mPlayerInfo of
    Nothing -> throwError err403
    Just playerInfo -> pure playerInfo


-- Generate a new game sheet id
type CreateNewSheetId =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "new-sheet-id"
  :> PostCreated '[JSON] UUID

createNewGameSheetId :: PersonId -> GameId -> App UUID
createNewGameSheetId myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    case access of
      Player ->
        -- permission denied
        throwError err403
      _ -> do
        muuid <- runDB $ DB.generateNewSheetUUID gameId
        case muuid of
          Nothing -> throwError err500
          Just uuid -> pure uuid


-- Delete a game sheet id
type DeleteSheetId =
  AuthProtect "cookie-auth"
  :> "games" :> Capture "gameId" GameId :> "sheet-id"
  :> Capture "sheetId" UUID
  :> Delete '[JSON] UUID

deleteGameSheetId :: PersonId -> GameId -> UUID -> App UUID
deleteGameSheetId myId gameId sheetId = do
  verifyGameAccess myId gameId $ \access -> do
    case access of
      Player ->
        -- permission denied
        throwError err403
      _ -> do
        result <- runDB $ DB.deleteSheetUUID gameId sheetId
        case result of
          Nothing -> throwError err500
          Just uuid -> pure uuid


verifyGameAccess :: PersonId
                 -> GameId
                 -> (AccessLevel -> App a) -> App a
verifyGameAccess myId gameId handler = do
  mAccess <- runDB $ DB.verifyGameAccess myId gameId
  case mAccess of
    Nothing -> throwError err403
    Just access -> handler access


couchProxy :: Config -> PersonId -> Application
couchProxy config myId request respond =
  case (requestMethod request, pathInfo request) of
    (_, "couchdb":"":[]) -> do
      waiProxyTo handler defaultOnExc manager request respond

    (_, "couchdb":rawGameId:_) -> do
      let
        toGameId =
          fmap GameId
          . UUID.fromText
          . T.drop 1
          . T.dropWhile (/= '_')

      case toGameId rawGameId of
        Nothing ->
          respond $ responseLBS notFound404 [] ""
        Just gameId -> do
          mAccess <- runDBwithConfig config $
                     DB.verifyGameAccess myId gameId
          case mAccess of
            Nothing ->
              respond $ responseLBS unauthorized401 [] ""
            Just access ->
              waiProxyTo handler defaultOnExc manager request respond

    _ -> respond $ responseLBS forbidden403 [] ""

  where
    handler request = do
      let newRequest =
            request
            { pathInfo = tail (pathInfo request)
            , rawPathInfo =
                BS.dropWhile (/= (fromIntegral . fromEnum $ '/'))
                . BS.drop 1
                $ rawPathInfo request
            }
      pure $
        WPRModifiedRequest newRequest $
        ProxyDest "127.0.0.1" 5984

    conn = getPGConnPool config
    redisConn = getRedisConn config
    manager = getProxyManager config
    
