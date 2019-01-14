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
  ( unauthorized401
  , forbidden403
  , notFound404
  )

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString as BS
import           Data.ByteString as BS (ByteString)
import qualified Data.UUID.Types as UUID
import           Data.UUID.Types (UUID)

import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Experimental.Auth

import Auth
import qualified Database as DB
import Database (runDB, runDBwithConfig)
import qualified CouchDB
import Types
import Config (Config(..))
import qualified Redis
import           Redis (redis)

type instance AuthServerData (AuthProtect "cookie-auth") = PersonId

type AuthAPI =
  "register"
      :> ReqBody '[JSON] Registration
      :> PostCreated '[JSON] PersonId

  :<|> "login"
      :> ReqBody '[JSON] AuthenticationData
      :> Post '[JSON] PersonId

  :<|> "logout"
      :> Post '[JSON] NoContent

type GamesAPI = "games" :>
  -- Get a list of all your games
    Get '[JSON] [Game]

    :<|> "games"
    -- Create a new game
    :> ReqBody '[JSON] NewGame
        :> PostCreated '[JSON] GameId

type InviteAPI =
  -- Use an invite to join a game
  "invite"
      :> Capture "inviteCode" InviteCode
      :> Post '[JSON] GameId

  -- Create an invite code for your game
  :<|> "invite" :> "games"
      :> Capture "gameId" GameId
      :> PutCreated '[JSON] InviteCode


type GameAPI = "games" :> Capture "gameId" GameId :>
  (-- Update the game title
    "title"
        :> ReqBody '[JSON] Text
        :> PostNoContent '[JSON] NoContent
    
    -- Generate a new game sheet uuid
    :<|> "new-sheet-id"
        :> PostCreated '[JSON] UUID

    -- Delete a game sheet uuid
    :<|> "sheet-id"
        :> Capture "sheetId" UUID
        :> Delete '[JSON] UUID
  )

type PlayerAPI = "games" :> Capture "gameId" GameId :>
  ("players" :>
    ( -- Get a list of all players in the game
      Get '[JSON] [Person]
    
      -- Add a player to the game
      :<|> ReqBody '[JSON] PersonId :> PostNoContent '[JSON] NoContent

      -- Remove a player from a game
      :<|> Capture "playerId" PersonId :> Delete '[JSON] NoContent
    )

    -- Get my player information
    :<|> "my-player-info" :> Get '[JSON] Person
  )

type ChatAPI = "games" :> Capture "gameId" GameId :> "chat" :>
  (-- Insert a new chat message
    PostCreated '[JSON] NoContent
  
    -- Get a chat log
    :<|> Get '[JSON] [ChatMessage]
  )


type RestAPI =
  AuthAPI
  :<|> AuthProtect "cookie-auth" :>
  (      GamesAPI
    :<|> GameAPI
    :<|> PlayerAPI
    :<|> ChatAPI
    :<|> InviteAPI
  )

type API = "api" :>
  (RestAPI
  
    -- CouchDB reverse proxy
    :<|> AuthProtect "cookie-auth" :> "couchdb" :> Raw
  )

--authServer :: ServerT AuthAPI App
authServer
  =    registerAccount
  :<|> loginAccount
  :<|> logoutAccount

--gamesServer :: ServerT GamesAPI App
gamesServer personId
  =    myGameList personId
  :<|> createNewGame personId

--gameServer :: ServerT GameAPI App
gameServer personId gameId
  =    updateGameTitle personId gameId
  :<|> newGameSheetId personId gameId
  :<|> deleteGameSheetId personId gameId

--playerServer :: GameId -> ServerT PlayerAPI App
playerServer personId gameId
  =    getPlayers personId gameId
  :<|> addPlayer personId gameId
  :<|> removePlayer personId gameId
  :<|> getMyPlayerInfo personId gameId

--chatServer :: GameId -> ServerT ChatAPI App
chatServer personId gameId
  =    newChatMessage personId gameId
  :<|> getChatLog personId gameId

--inviteServer :: GameId -> ServerT InviteAPI App
inviteServer personId gameId
  =    createGameInvite personId gameId
  :<|> useGameInvite personId

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

--server :: ServerT API App
server config =
  hoistServerWithContext api authContextProxy (nt config)
  (      authServer
    :<|> gamesServer
    :<|> gameServer
    :<|> playerServer
    :<|> chatServer
    :<|> inviteServer
    :<|> Tagged (couchProxy config)
  )

-- Register a user account
registerAccount :: Registration -> App PersonId
registerAccount reg = do
    mPersonId <- runDB $ DB.createPerson reg
    case mPersonId of
      Nothing -> throwError err409
      Just personId -> pure personId

-- Log in to a user account
loginAccount :: AuthenticationData -> App PersonId
loginAccount authData = do
    mPersonId <- runDB $ DB.verifyAuthentication authData
    case mPersonId of
      Nothing -> throwError err400
      Just personId -> do
        createSession personId
        pure personId

-- Log out of a user account
logoutAccount :: App NoContent
logoutAccount = do
    -- mSessionId <- getSessionId <$> request
    -- case mSessionId of
    --   Nothing -> throwError err404
    --   Just sessionId -> do
    --     deleteSession sessionId
        pure NoContent

-- Get a list of all games for a user account
myGameList :: PersonId -> App [Game]
myGameList myId =
  runDB $ DB.getGamesForPersonId myId

-- Create a new game
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
updateGameTitle :: PersonId -> GameId -> Text -> App NoContent
updateGameTitle myId gameId title = do
  verifyGameAccess myId gameId $ \access -> do
    mResult <- runDB $ DB.updateGameTitle gameId title
    case mResult of
      Nothing -> throwError $
        err500 { errBody = "Could not update game title" }
      Just _ -> pure NoContent


-- Get a list of all players in a game
getPlayers :: PersonId -> GameId -> App [Person]
getPlayers myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    runDB $ DB.listPeopleInGame gameId


-- Add a player to a game
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

-- Create an invite to a game
createGameInvite :: PersonId -> GameId -> App InviteCode
createGameInvite myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    createInvite gameId

-- Use an invite to join a game
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
        _ ->
          throwError err404

-- Remove a player from a game
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
getChatLog :: PersonId -> GameId -> App [ChatMessage]
getChatLog myId gameId = do
  verifyGameAccess myId gameId $ \access -> do
    runDB $ DB.getChatLog gameId 50


-- Get my player information
getMyPlayerInfo :: PersonId -> GameId -> App Person
getMyPlayerInfo myId gameId = do
  mPlayerInfo <- runDB $ DB.getPlayerInfo myId gameId
  case mPlayerInfo of
    Nothing -> throwError err403
    Just playerInfo -> pure playerInfo


-- Generate a new game sheet id
newGameSheetId :: PersonId -> GameId -> App UUID
newGameSheetId myId gameId = do
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
    
