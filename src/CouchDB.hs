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

module CouchDB
  ( createDatabase )
where

import Data.Aeson
import Data.Default.Class
import Network.HTTP.Req
import Types (GameId)
import qualified Data.Text as T (pack)
import qualified Data.ByteString.Lazy as LBS (ByteString)

couchDomain = "localhost"
couchPort = 5984

createDatabase :: GameId -> LBS.ByteString -> IO (Maybe ())
createDatabase gameId gameData =
  runReq def $ do
  r <- req PUT
    (http couchDomain /: T.pack (show gameId))
    NoReqBody
    ignoreResponse
    (port couchPort)
  -- pure $
  --   case responseStatusCode r of
  --     201 -> Just ()
  --     _   -> Nothing
  case responseStatusCode r of
    201 ->
      runReq def $ do
      r2 <- req POST
        (http couchDomain /: T.pack (show gameId))
        (ReqBodyLbs gameData)
        ignoreResponse
        (port couchPort
         <> header "Content-Type" "application/json")
      case responseStatusCode r2 of
        201 ->
          pure $ Just ()
        _ ->
          pure Nothing
    _   ->
      pure Nothing
