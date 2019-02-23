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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where

import           Data.Text (Text)
import qualified Data.Text as T (dropWhile, drop)
import           Data.Text.Lazy as LT (toStrict)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import           Data.Binary.Builder (fromByteString)

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , genericToEncoding
  , genericParseJSON
  , defaultOptions
  , Options(..)
  , (.:)
  )
import           Data.Int (Int64)
import qualified Data.Char as Char
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID (fromText, toText)
import           Data.Time (UTCTime)

import GHC.Generics
import Control.Applicative (empty, (<|>))
import Control.Monad.Trans.Reader (ReaderT)

import Web.HttpApiData (FromHttpApiData(..))
import Servant (Handler, MimeRender(..), Accept(..))
import Network.HTTP.Media ((//), (/:))
import qualified Data.List.NonEmpty as NE

import Config (Config)



type App = ReaderT Config Handler

data RawJSON

instance Accept RawJSON where
  contentTypes _ =
    "application" // "json" /: ("charset", "utf-8") NE.:|
    [ "application" // "json" ]

instance MimeRender RawJSON ByteString where
  mimeRender _ bs = LBS.fromStrict bs

toJSONFieldName :: String -> String -> String
toJSONFieldName prefix fieldName =
  case drop (length prefix) fieldName of
    a:as -> Char.toLower a : as
    as -> as

fromJSONFieldName :: String -> String -> String
fromJSONFieldName prefix fieldName =
  prefix ++ capitalize fieldName
  where
    capitalize s =
      case s of
        a:as -> Char.toUpper a : as
        as -> as


------------------------------------------------------------
-- Chat Message
------------------------------------------------------------

data ChatMessageType
  = ChatMessageType
  | DiceRollType
    deriving (Generic)

instance ToJSON ChatMessageType
instance FromJSON ChatMessageType

data ChatMessage = ChatMessage
  { _chatMessageType       :: ChatMessageType
  , _chatMessageTimestamp  :: UTCTime
  , _chatMessagePlayerId   :: PersonId
  , _chatMessagePlayerName :: Text
  , _chatMessageContent    :: Value
  } deriving (Generic)

chatMessagePrefix = "_chatMessage"

instance FromJSON ChatMessage where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName chatMessagePrefix }

instance ToJSON ChatMessage where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = toJSONFieldName chatMessagePrefix }

------------------------------------------------------------
-- New Chat Message
------------------------------------------------------------

data NewChatMessage
  = NewChatMessage Value
  | NewDiceRoll Value

instance FromJSON NewChatMessage where
  parseJSON (Object v) = do
    ctor :: Text <- v .: "type"
    case ctor of
      "ChatMessageType" -> NewChatMessage <$> v .: "content"
      "DiceRollType" -> NewDiceRoll <$> v .: "content"
      _ -> empty

------------------------------------------------------------
-- Access Level
------------------------------------------------------------

data AccessLevel
  = Player
  | GameMaster
  | Owner
  deriving (Generic, Eq)

instance ToJSON AccessLevel
instance FromJSON AccessLevel

instance Show AccessLevel where
  show acl = case acl of
    Player     -> "Player"
    GameMaster -> "Game Master"
    Owner      -> "Owner"

------------------------------------------------------------
-- Registration
------------------------------------------------------------

data Registration = Registration
  { _registrationUsername :: Text
  , _registrationEmail    :: Text
  , _registrationPassword :: Text
  } deriving (Show, Generic)

instance FromJSON Registration where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName "_registration" }

------------------------------------------------------------
-- Login
------------------------------------------------------------

data Login = Login
  { _loginEmail    :: Text
  , _loginPassword :: Text
  } deriving (Show, Generic)

instance FromJSON Login where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName "_login" }


------------------------------------------------------------
-- Auth
------------------------------------------------------------

data Authentication = Authentication
  { _authId       :: PersonId
  , _authEmail    :: Text
  , _authPassword :: Text
  } deriving (Show, Generic)

instance FromJSON Authentication where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName "_auth" }

------------------------------------------------------------
-- PersonId
------------------------------------------------------------

newtype PersonId = PersonId
  { unPersonId :: Text
  } deriving newtype
      (Show, Read, FromHttpApiData, FromJSON, ToJSON, Eq, Ord)

------------------------------------------------------------
-- Person
------------------------------------------------------------

data Person = Person
  { _personId       :: PersonId
  , _personUsername :: Text
  , _personEmail    :: Text
  , _personCreated  :: UTCTime
  } deriving (Show, Generic)

personPrefix = "_person"

instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName personPrefix }

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = toJSONFieldName personPrefix }

------------------------------------------------------------
-- New Person
------------------------------------------------------------

data NewPerson = NewPerson
  { _personUsername :: Text
  , _personEmail    :: Text
  , _personPassword :: Text
  , _personCreated  :: UTCTime
  } deriving (Show, Generic)

instance ToJSON NewPerson where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = toJSONFieldName "_person" }

------------------------------------------------------------
-- GameId
------------------------------------------------------------

newtype GameId = GameId
  { unGameId :: UUID
  } deriving newtype (Show, FromHttpApiData, FromJSON, ToJSON)

------------------------------------------------------------
-- Game
------------------------------------------------------------

data Game = Game
  { _gameId      :: GameId
  , _gameTitle   :: Text
  , _gameType    :: Text
  , _gameCreated :: UTCTime
  } deriving (Show, Generic)

gamePrefix = "_game"

instance FromJSON Game where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName gamePrefix }

instance ToJSON Game where
  toEncoding = genericToEncoding defaultOptions
    { fieldLabelModifier = toJSONFieldName gamePrefix }

------------------------------------------------------------
-- NewGame
------------------------------------------------------------

data NewGame = NewGame
  { _newGameTitle :: Text
  , _newGameType :: Text
  } deriving (Show, Generic)

newGamePrefix = "_newGame"

instance FromJSON NewGame where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = fromJSONFieldName newGamePrefix }

------------------------------------------------------------
-- Invite Code
------------------------------------------------------------

newtype InviteCode = InviteCode
  { unInviteCode :: Text
  } deriving newtype
      (Show, FromHttpApiData, FromJSON, ToJSON, Read, Eq, Ord)
