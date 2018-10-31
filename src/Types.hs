{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.=)
  , object
  )
import Control.Applicative (empty)
import Data.Int (Int64)
-- import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
-- import Data.Time (UTCTime)
-- import GHC.Generics

------------------------------------------------------------
-- Registration
------------------------------------------------------------

data Registration = Registration
  { _registrationUsername :: Text
  , _registrationEmail :: Text
  , _registrationPassword :: Text
  } deriving (Show)

instance ToJSON Registration where
  toJSON (Registration username email password)
    = object
      [ "username" .= username
      , "email" .= email
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
  { _authEmail :: Text
  , _authPassword :: Text
  } deriving (Show)

instance ToJSON AuthenticationData where
  toJSON (AuthenticationData email password)
    = object
      [ "email" .= email
      , "password" .= password
      ]

instance FromJSON AuthenticationData where
  parseJSON (Object v)
    = AuthenticationData
      <$> v .: "email"
      <*> v .: "password"
  
  parseJSON _ = empty

------------------------------------------------------------
-- User data
------------------------------------------------------------

data Person = Person
  { _personId :: PersonId
  , _personUsername :: Text
  } deriving (Show)

instance ToJSON Person where
  toJSON (Person (PersonId id_) username)
    = object
      [ "id" .= id_
      , "username" .= username
      ]


newtype PersonId = PersonId
  { unPersonId :: Int64
  }

instance Show PersonId where
  show (PersonId id_) = show id_

instance FromField PersonId where
  fromField f metadata = PersonId <$> fromField f metadata

instance FromRow PersonId where
  fromRow = PersonId
    <$> field

instance ToJSON PersonId where
  toJSON (PersonId id_) = toJSON id_

------------------------------------------------------------
-- Game
------------------------------------------------------------

newtype GameId = GameId
  { unGameId :: Text
  } deriving (Show)

instance FromField GameId where
  fromField f metadata = GameId <$> fromField f metadata

instance FromRow GameId where
  fromRow = GameId
    <$> field

instance ToJSON GameId where
  toJSON (GameId id_) = toJSON id_

data Game = Game
  { _gameId :: GameId
  , _gameTitle :: Text
  } deriving (Show)

instance FromRow Game where
  fromRow = Game
    <$> field
    <*> field

instance ToJSON Game where
  toJSON (Game id_ title)
    = object
      [ "id" .= id_
      , "title" .= title
      ]


data NewGame = NewGame
  { _newGameTitle :: Text
  } deriving (Show)

instance FromJSON NewGame where
  parseJSON (Object v)
    = NewGame
      <$> v .: "title"

  parseJSON _ = empty
