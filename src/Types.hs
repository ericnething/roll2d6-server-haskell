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
  )
where

import           Data.Text (Text)
import qualified Data.Text as T (dropWhile, drop)
import Data.Text.Lazy as LT (toStrict)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.=)
  , object
  , genericToEncoding
  , defaultOptions
  )
import Control.Applicative (empty)
import Data.Int (Int64)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID (fromText, toText)
import Web.Scotty (Parsable(..))


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
  } deriving (Show)

instance ToJSON Registration where
  toJSON (Registration username email password)
    = object
      [ "username" .= username
      , "email"    .= email
      , "password" .= password
      ]

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
  } deriving (Show)

instance ToJSON AuthenticationData where
  toJSON (AuthenticationData email password)
    = object
      [ "email"    .= email
      , "password" .= password
      ]

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
      (FromField, ToField, FromJSON, ToJSON, Parsable, Read)

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
  } deriving (Show)

instance ToJSON Person where
  toJSON (Person (PersonId id_) username access)
    = object
      [ "id"       .= id_
      , "username" .= username
      , "access"   .= access
      ]

instance FromRow Person where
  fromRow = Person
    <$> field
    <*> field
    <*> field

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

instance Show GameId where
  show (GameId uuid) = "game_" <> show uuid

------------------------------------------------------------
-- Game
------------------------------------------------------------

data Game = Game
  { _gameId    :: GameId
  , _gameTitle :: Text
  } deriving (Show)

instance FromRow Game where
  fromRow = Game
    <$> field
    <*> field

instance ToJSON Game where
  toJSON (Game id_ title)
    = object
      [ "id"    .= id_
      , "title" .= title
      ]

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
