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

module Arango
  ( )
where

import qualified Data.Aeson as Aeson
import Data.Aeson
  ( Value
  , (.=)
  )
import Data.Default.Class
import Network.HTTP.Client
  ( Manager
  , newManager
  , defaultManagerSettings
  )
import Network.HTTP.Req
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString.Lazy as LBS (ByteString)

data Context = Context
  { contextManager :: Manager
    ,
  }

toQuery :: Text -> Encoding -> Encoding
toQuery q bindVars = Aeson.encodingToLazyByteString
  Aeson.pairs
  ( "query" .= q <>
    "bindVars" .= bindVars
  )

data Connection = Connection
  { _connectionHost :: Text
  , _connectionPort :: Int
  }

newtype ArangoM a = ArangoM { unArangoM :: Connection -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT Connection IO)


runArango :: ArangoQuery -> IO a
runArango q =
  runReq def $ do
  r <- req POST
       (http "localhost")
       (ReqBodyLbs (encode newGame))
       jsonResponse
       (port "8529" <>
        header "Content-Type" "application/json")
  pure $
    case responseStatusCode r of
      201 -> pure $ Just ()
      _ -> pure Nothing
