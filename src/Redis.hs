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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module Redis
  ( redis
  , redisWithConfig
  , lookupSession
  , createSession
  , deleteSession
  , createInvite
  , lookupGameInvite
  )
where

import Data.Monoid ((<>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Trans.Reader (ReaderT(..))

import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)

import Database.Redis
import Types
  ( PersonId(..)
  , GameId(..)
  , InviteCode(..)
  )
import Config (Config(..))

newtype RedisQuery a = RedisQuery
  { unRedisQuery :: Connection -> IO a
  } deriving (Functor, Applicative, Monad) via (ReaderT Connection IO)

redis :: (MonadIO m, MonadReader Config m) => RedisQuery a -> m a
redis q = do
  conn <- asks getRedisConn
  liftIO ((unRedisQuery q) conn)

redisWithConfig :: MonadIO m => Config -> RedisQuery a -> m a
redisWithConfig config q = do
  let conn = getRedisConn config
  liftIO ((unRedisQuery q) conn)

lookupSession :: Integer
              -> ByteString
              -> RedisQuery (Either LBS.ByteString PersonId)
lookupSession expiration sessionId = RedisQuery $ \conn -> do
  emPersonId <- runRedis conn $ do
    session  <- get sessionId
    ettl     <- ttl sessionId
    let
      extendTTL =
        case ettl of
          Left _ ->
            False
          Right ttl ->
            ttl > 0 && ttl < expiration `div` 2
    when extendTTL $
      expire sessionId expiration >>
      pure ()
    pure session
  pure $
    case emPersonId of
      Right (Just personId) ->
        Right $ read (BS8.unpack personId)
      _ ->
        Left "Invalid session cookie"

createSession :: ByteString
              -> PersonId
              -> Integer
              -> RedisQuery ByteString
createSession sessionId personId exp = RedisQuery $ \conn -> do
  runRedis conn $ do
    set sessionId (BS8.pack . show $ personId)
    expire sessionId exp
  pure sessionId


deleteSession :: ByteString -> RedisQuery ()
deleteSession sessionId = RedisQuery $ \conn -> do
  runRedis conn $ do
    del [ sessionId ]
  -- setAuthCookie "" 1
  pure ()


createInvite :: GameId -> ByteString -> Integer -> RedisQuery ()
createInvite (GameId gameId) inviteId exp = RedisQuery $ \conn -> do
  emStatus <- runRedis conn $ do
    set inviteId (BS8.pack . show $ gameId)
    expire inviteId exp
  pure ()

lookupGameInvite :: InviteCode -> RedisQuery (Either Reply (Maybe ByteString))
lookupGameInvite (InviteCode inviteId) = RedisQuery $ \conn -> do
  runRedis conn (get ("invite:" <> T.encodeUtf8 inviteId))
