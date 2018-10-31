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

-- getAllUsers :: Connection -> IO [Person]
-- getAllUsers conn = query_ conn sql
--   where
--     sql =
--       "SELECT id, username \
--       \FROM person;"

-- getUserById :: Connection -> Int64 -> IO (Maybe User)
-- getUserById conn uid = do
--   muser <- query conn sql (Only uid)
--   return $ case muser of
--     []  -> Nothing
--     u:_ -> Just u
--   where
--     sql =
--       "SELECT id, first_name, last_name, email, access, active \
--       \FROM person \
--       \WHERE id = ?;"

-- getAllGames :: Connection -> IO [GameId]
-- getAllGames conn = query_ conn sql
--   where
--     sql =
--       "SELECT database \
--       \FROM game \
--       \ORDER BY created_at ASC;"

createPerson :: Connection -> Registration -> IO (Maybe PersonId)
createPerson conn (Registration username email password) = do
  mPersonId <- query conn sql (username, email, password)
  return $
    case mPersonId of
      [] -> Nothing
      personId:_ -> Just personId
  where
    sql =
      "INSERT INTO person \
      \  (username, email, password) VALUES \
      \  (?, ?, crypt(?, gen_salt('bf', 8)))\
      \  RETURNING id;"

verifyAuthentication :: Connection
                     -> AuthenticationData
                     -> IO (Maybe PersonId)
verifyAuthentication conn (AuthenticationData email password) = do
  mAuthenticated <- query conn sql (email, password)
  return $
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
getGamesForPersonId conn (PersonId personId) =
  query conn sql (Only personId)
  where
    sql =
      "SELECT database, title \
      \  FROM game\
      \  WHERE person_id = ?;"

createGameForPersonId :: Connection -> PersonId -> Text -> IO GameId
createGameForPersonId conn (PersonId personId) title = do
  [gameId] <- query conn sql (personId, title)
  return gameId
  where
    sql =
      "INSERT INTO game\
      \  (person_id, title) VALUES\
      \  (?, ?)\
      \  RETURNING database;"

