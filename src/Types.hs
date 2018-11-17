{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types
  ( AccessLevel(..)
  , Registration(..)
  , AuthenticationData(..)
  , Person(..)
  , PersonId(..)
  , Game(..)
  , GameId(..)
  , NewGame(..)
  , updatePresence
  , PersonPresence(..)
  )
where

import           Data.Text (Text)
import qualified Data.Text as T (dropWhile, drop)
import           Data.Text.Lazy as LT (toStrict)

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.=)
  , object
  , genericToEncoding
  , defaultOptions
  , pairs
  )
import Data.Aeson.Encoding (string)

import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Int (Int64)

import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID (fromText, toText)

import Web.Scotty (Parsable(..))
import GHC.Generics
import Control.Applicative (empty)
import qualified Data.Map.Strict as Map (member)
import           Data.Map.Strict (Map)
import           Data.Time (UTCTime)


------------------------------------------------------------
-- Access Level
------------------------------------------------------------

data AccessLevel
  = Player
  | GameMaster
  | Owner
  deriving (Generic, Eq)

instance ToJSON AccessLevel

instance Show AccessLevel where
  show acl = case acl of
    Player     -> "Player"
    GameMaster -> "Game Master"
    Owner      -> "Owner"

instance FromField AccessLevel where
  fromField f mdata = case mdata of
    Just "player"      -> pure Player
    Just "game_master" -> pure GameMaster
    Just "owner"       -> pure Owner
    Just _ -> returnError ConversionFailed f
              "Unrecognized value for AccessLevel"
    _ -> returnError UnexpectedNull f "Null value for AccessLevel"

instance ToField AccessLevel where
  toField acl = case acl of
    Player     -> Escape "player"
    GameMaster -> Escape "game_master"
    Owner      -> Escape "owner"

------------------------------------------------------------
-- Registration
------------------------------------------------------------

data Registration = Registration
  { _registrationUsername :: Text
  , _registrationEmail    :: Text
  , _registrationPassword :: Text
  } deriving (Show, Generic)

instance ToJSON Registration where
  toEncoding (Registration username email password)
    = pairs
      (  "username" .= username
      <> "email"    .= email
      <> "password" .= password
      )

instance FromJSON Registration where
  parseJSON (Object v)
    = Registration
      <$> v .: "username"
      <*> v .: "email"
      <*> v .: "password"
  
  parseJSON _ = empty

------------------------------------------------------------
-- Login
------------------------------------------------------------

data AuthenticationData = AuthenticationData
  { _authEmail    :: Text
  , _authPassword :: Text
  } deriving (Show, Generic)

instance ToJSON AuthenticationData where
  toEncoding (AuthenticationData email password)
    = pairs
      (  "email"    .= email
      <> "password" .= password
      )

instance FromJSON AuthenticationData where
  parseJSON (Object v)
    = AuthenticationData
      <$> v .: "email"
      <*> v .: "password"
  
  parseJSON _ = empty

------------------------------------------------------------
-- PersonId
------------------------------------------------------------

newtype PersonId = PersonId
  { unPersonId :: Int64
  } deriving newtype
      (FromField, ToField, FromJSON, ToJSON, Parsable, Read, Eq, Ord)

instance Show PersonId where
  show (PersonId id_) = show id_

instance FromRow PersonId where
  fromRow = PersonId
    <$> field

------------------------------------------------------------
-- Person
------------------------------------------------------------

data Person = Person
  { _personId       :: PersonId
  , _personUsername :: Text
  , _personAccess   :: AccessLevel
  , _personPresence :: Bool
  } deriving (Show, Generic)

instance ToJSON Person where
  toEncoding (Person (PersonId id_) username access presence)
    = pairs
      (  "id"       .= id_
      <> "username" .= username
      <> "access"   .= access
      <> "presence" .= if presence
                       then "online" :: Text
                       else "offline"
      )

instance FromRow Person where
  fromRow = (\a b c -> Person a b c False)
    <$> field
    <*> field
    <*> field

updatePresence :: Map PersonId UTCTime -> Person -> Person
updatePresence presence person =
  person { _personPresence = Map.member pid presence }
  where
    pid = _personId $ person

data PersonPresence = PersonPresence PersonId Bool
  deriving (Generic)

instance ToJSON PersonPresence where
  toEncoding (PersonPresence (PersonId id_) presence)
    = pairs
    (  "id" .= id_
    <> "presence" .= if presence
                     then "online" :: Text
                     else "offline"
    )

------------------------------------------------------------
-- GameId
------------------------------------------------------------

newtype GameId = GameId
  { unGameId :: UUID
  } deriving newtype (FromField, ToField)

instance FromRow GameId where
  fromRow = GameId
    <$> field

instance Parsable GameId where
  parseParam s =
    case UUID.fromText (stripPrefix s) of
      Nothing ->
        Left "Failed to convert text into uuid."
      Just uuid ->
        Right (GameId uuid)
    where
      stripPrefix
        = T.drop 1
        . T.dropWhile (/= '_')
        . LT.toStrict

instance ToJSON GameId where
  toJSON (GameId uuid) = toJSON ("game_" <> (show uuid))
  toEncoding (GameId uuid) = string ("game_" <> (show uuid))

instance Show GameId where
  show (GameId uuid) = "game_" <> show uuid

------------------------------------------------------------
-- Game
------------------------------------------------------------

data Game = Game
  { _gameId    :: GameId
  , _gameTitle :: Text
  } deriving (Show, Generic)

instance FromRow Game where
  fromRow = Game
    <$> field
    <*> field

instance ToJSON Game where
  toEncoding (Game id_ title)
    = pairs
      (  "id"    .= id_
      <> "title" .= title
      )

------------------------------------------------------------
-- NewGame
------------------------------------------------------------

data NewGame = NewGame
  { _newGameTitle :: Text
  } deriving (Show)

instance FromJSON NewGame where
  parseJSON (Object v)
    = NewGame
      <$> v .: "title"

  parseJSON _ = empty

