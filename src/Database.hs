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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Database
  ( PGQuery
  , runDB
  , runDBwithConfig
  , createPerson
  , verifyAuthentication
  , getGamesForPersonId
  , createGameForPersonId
  , listPeopleInGame
  , addPersonToGame
  , removePersonFromGame
  , verifyGameAccess
  , insertChatMessage
  , getChatLog
  , generateNewSheetUUID
  , deleteSheetUUID
  , getPlayerInfo
  , updateGameTitle
  )
where

import Types
import Database.PostgreSQL.Simple
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Aeson as Json (encode)
import qualified Data.UUID.Types as UUID
import           Data.UUID.Types (UUID)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Reader (ReaderT(..))

import Config (Config(..))
import Data.Pool (withResource)


newtype PGQuery a = PGQuery { unPGQuery :: Connection -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT Connection IO)

runDB :: (MonadIO m, MonadReader Config m) => PGQuery a -> m a
runDB q = do
  pool <- asks getPGConnPool
  liftIO (withResource pool (unPGQuery q))

runDBwithConfig :: MonadIO m => Config -> PGQuery a -> m a
runDBwithConfig config q = do
  let pool = getPGConnPool config
  liftIO (withResource pool (unPGQuery q))

createPerson :: Registration -> PGQuery (Maybe PersonId)
createPerson reg = PGQuery $ \conn -> do
  let
    username = _registrationUsername reg
    email    = _registrationEmail reg
    password = _registrationPassword reg

  mPersonId <- query conn sql (username, email, password)
  pure $
    case mPersonId of
      [] -> Nothing
      personId:_ -> Just personId
  where
    sql =
      "INSERT INTO person (username, email, password) \
      \  VALUES (?, ?, crypt(?, gen_salt('bf', 8)))\
      \  RETURNING id;"

verifyAuthentication :: AuthenticationData -> PGQuery (Maybe PersonId)
verifyAuthentication auth = PGQuery $ \conn -> do
  let
    email = _authEmail auth
    password = _authPassword auth

  mAuthenticated <- query conn sql (email, password)
  pure $
    case mAuthenticated of
      [] -> Nothing
      personId:_ -> Just personId
  where
    sql =
      "SELECT id \
      \  FROM person \
      \  WHERE email = ? \
      \  AND password = crypt(?, password);"

getGamesForPersonId :: PersonId -> PGQuery [Game]
getGamesForPersonId personId = PGQuery $ \conn ->
  query conn sql (Only personId)
  where
    sql =
      "SELECT game.id, game.title \
      \  FROM person_game_relation as rel \
      \  INNER JOIN game \
      \  ON game.id = rel.game_id \
      \  WHERE rel.person_id = ? \
      \  ORDER BY game.created_at ASC;"

createGameForPersonId :: PersonId -> NewGame -> PGQuery GameId
createGameForPersonId personId newGame = PGQuery $ \conn -> do
  [gameId] <- query conn sql
              ( _newGameTitle newGame
              , _newGameType newGame
              , personId
              )
  pure gameId
  where
    sql =
      "WITH new_game AS ( \
      \  INSERT INTO game (title, gameType) \
      \    VALUES (?, ?) \
      \    RETURNING id \
      \), rel AS ( \
      \  INSERT INTO person_game_relation \
      \    (game_id, person_id, access) \
      \    (SELECT id, ?, 'owner' FROM new_game) \
      \) \
      \SELECT id FROM new_game;"


listPeopleInGame :: GameId -> PGQuery [Person]
listPeopleInGame gameId = PGQuery $ \conn ->
  query conn sql (Only gameId)
  where
    sql =
      "SELECT person.id, person.username, rel.access \
      \  FROM person_game_relation as rel \
      \  INNER JOIN person \
      \  ON person.id = rel.person_id \
      \  WHERE rel.game_id = ? \
      \  ORDER BY rel.created_at ASC;"


addPersonToGame :: PersonId -> GameId -> PGQuery (Maybe Bool)
addPersonToGame personId gameId = PGQuery $ \conn -> do
  result <- query conn sql (personId, gameId)
  pure $
    case result of
      []           -> Nothing
      (Only bool):_ -> Just bool
  where
    sql =
      "INSERT INTO person_game_relation \
      \ (person_id, game_id) \
      \  VALUES (?, ?) \
      \  RETURNING true;"


removePersonFromGame :: PersonId -> GameId -> PGQuery (Maybe Bool)
removePersonFromGame personId gameId = PGQuery $ \conn -> do
  result <- query conn sql (personId, gameId)
  pure $
    case result of
      []           -> Nothing
      (Only bool):_ -> Just bool
  where
    sql =
      "DELETE FROM person_game_relation \
      \  WHERE person_id = ? \
      \  AND game_id = ? \
      \  RETURNING true;"


verifyGameAccess :: PersonId -> GameId -> PGQuery (Maybe AccessLevel)
verifyGameAccess personId gameId = PGQuery $ \conn -> do
  result <- query conn sql (personId, gameId)
  pure $
    case result of
      []           -> Nothing
      (Only acl):_ -> Just acl
  where
    sql =
      "SELECT access \
      \  FROM person_game_relation \
      \  WHERE person_id = ? \
      \  AND game_id = ?;"


insertChatMessage :: PersonId
                  -> GameId
                  -> NewChatMessage
                  -> PGQuery (Maybe ChatMessage)
insertChatMessage personId gameId message = PGQuery $ \conn -> do
  result <- case message of
    NewChatMessage body ->
      query conn (sql "body")
        (personId, gameId, ChatMessageType, body)

    NewDiceRollMessage diceResult ->
      query conn (sql "dice_result")
        (personId, gameId, DiceRollMessageType, diceResult)

  pure $
    case result of
      []            -> Nothing
      chatMessage:_ -> Just chatMessage

  where
    sql bodyOrResult =
      "WITH message as ( \
      \    INSERT INTO chat_message \
      \    (person_id, game_id, ctor, " <> bodyOrResult <> ") \
      \    VALUES (?, ?, ?, ?) \
      \    RETURNING * \
      \) \
      \  SELECT m.ctor, m.created_at, m.person_id, \
      \         p.username, m.body, m.dice_result \
      \  FROM message as m \
      \  INNER JOIN person as p \
      \  ON p.id = m.person_id;"


getChatLog :: GameId -> Int64 -> PGQuery [ChatMessage]
getChatLog gameId limit = PGQuery $ \conn -> do
  query conn sql (gameId, limit)
  where
    sql =
      "SELECT ctor, chat.created_at, person_id, \
      \       person.username, body, dice_result \
      \  FROM chat_message as chat\
      \  INNER JOIN person as person \
      \  ON person.id = person_id \
      \  WHERE game_id = ? \
      \  ORDER BY created_at DESC \
      \  LIMIT ?;"


generateNewSheetUUID :: GameId -> PGQuery (Maybe UUID)
generateNewSheetUUID gameId = PGQuery $ \conn -> do
  result <- query conn sql (Only gameId)
  pure $
    case result of
      [] ->
        Nothing
      (Only uuid):_ ->
        Just uuid
  where
    sql =
      "INSERT INTO game_sheet \
      \ (game_id) \
      \  VALUES (?) \
      \  RETURNING sheet_id;"


deleteSheetUUID :: GameId -> UUID -> PGQuery (Maybe UUID)
deleteSheetUUID gameId sheetId = PGQuery $ \conn -> do
  result <- query conn sql (gameId, sheetId)
  pure $
    case result of
      [] ->
        Nothing
      (Only uuid):_ ->
        Just uuid
  where
    sql =
      "DELETE FROM game_sheet \
      \  WHERE game_id = ? \
      \  AND sheet_id = ? \
      \  RETURNING sheet_id;"


getPlayerInfo :: PersonId -> GameId -> PGQuery (Maybe Person)
getPlayerInfo personId gameId = PGQuery $ \conn -> do
  result <- query conn sql (gameId, personId)
  pure $
    case result of
      [] ->
        Nothing
      person:_ ->
        Just person
  where
    sql =
      "SELECT person.id, person.username, rel.access \
      \  FROM person_game_relation as rel \
      \  INNER JOIN person \
      \  ON person.id = rel.person_id \
      \  WHERE rel.game_id = ? \
      \  AND person.id = ?;"


updateGameTitle :: GameId -> Text -> PGQuery (Maybe Bool)
updateGameTitle gameId title = PGQuery $ \conn -> do
  result <- query conn sql (title, gameId)
  pure $
    case result of
      [] ->
        Nothing
      (Only bool):_ ->
        Just bool
  where
    sql =
      "UPDATE game \
      \  SET title = ? \
      \  WHERE id = ? \
      \  RETURNING true;"
