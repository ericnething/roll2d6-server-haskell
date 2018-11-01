{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import Types
import Database.PostgreSQL.Simple
import Data.Int (Int64)
import Data.Text (Text)

getConnection :: IO Connection
getConnection = connect $ ConnectInfo
  { connectHost     = "localhost"
  , connectPort     = 5432
  , connectUser     = "faterpg"
  , connectPassword = "faterpg"
  , connectDatabase = "faterpg"
  }

createPerson :: Connection -> Registration -> IO (Maybe PersonId)
createPerson conn (Registration username email password) = do
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

verifyAuthentication :: Connection
                     -> AuthenticationData
                     -> IO (Maybe PersonId)
verifyAuthentication conn (AuthenticationData email password) = do
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

getGamesForPersonId :: Connection -> PersonId -> IO [Game]
getGamesForPersonId conn personId =
  query conn sql (Only personId)
  where
    sql =
      "SELECT game.id, game.title \
      \  FROM person_game_relation as rel \
      \  INNER JOIN game \
      \  ON game.id = rel.game_id \
      \  WHERE rel.person_id = ? \
      \  ORDER BY game.created_at ASC;"

createGameForPersonId :: Connection -> PersonId -> Text -> IO GameId
createGameForPersonId conn personId title = do
  [gameId] <- query conn sql (title, personId)
  pure gameId
  where
    sql =
      "WITH new_game AS ( \
      \  INSERT INTO game (title) \
      \    VALUES (?) \
      \    RETURNING id \
      \), rel AS ( \
      \  INSERT INTO person_game_relation \
      \    (game_id, person_id, access) \
      \    (SELECT id, ?, 'owner' FROM new_game) \
      \) \
      \SELECT id FROM new_game;"


listPeopleInGame :: Connection -> GameId -> IO [Person]
listPeopleInGame conn gameId =
  query conn sql (Only gameId)
  where
    sql =
      "SELECT person.id, person.username, rel.access \
      \  FROM person_game_relation as rel \
      \  INNER JOIN person \
      \  ON person.id = rel.person_id \
      \  WHERE rel.game_id = ? \
      \  ORDER BY rel.created_at ASC;"


addPersonToGame :: Connection
                -> PersonId
                -> GameId
                -> IO (Maybe Bool)
addPersonToGame conn personId gameId = do
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


removePersonFromGame :: Connection
                     -> PersonId
                     -> GameId
                     -> IO (Maybe Bool)
removePersonFromGame conn personId gameId = do
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


verifyGameAccess :: Connection
                 -> PersonId
                 -> GameId
                 -> IO (Maybe Bool)
verifyGameAccess conn personId gameId = do
  result <- query conn sql (personId, gameId)
  pure $
    case result of
      []           -> Nothing
      (Only bool):_ -> Just bool
  where
    sql =
      "SELECT true \
      \  FROM person_game_relation \
      \  WHERE person_id = ? \
      \  AND game_id = ?;"
