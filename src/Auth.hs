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
{-# LANGUAGE FlexibleContexts #-}

module Auth
  ( createSession
  , deleteSession
  , getSessionId
  , createInvite
  , authHandler
  )
where

import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Network.Wai (Request, requestHeaders)
import Web.Cookie
  ( parseCookies
  , defaultSetCookie
  , SetCookie(..)
  )
import Servant
  ( Handler
  , ServantErr(..)
  , throwError
  , err401
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , mkAuthHandler
  )
import qualified Data.Text.Encoding as T (decodeUtf8)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64 (encode)
import qualified Data.ByteString.Base16 as Base16 (encode)
import           System.Entropy (getEntropy)
import Data.Time.Clock (secondsToDiffTime)
import Types
  ( App
  , PersonId(..)
  , GameId(..)
  , InviteCode(..)
  )
import Config (Config)
import qualified Redis
import           Redis (redis, redisWithConfig)

expiration :: Integer
expiration = minutes 20

hours :: Integer -> Integer
hours h = 3600 * h

minutes :: Integer -> Integer
minutes m = 60 * m

oneYear :: Integer
oneYear = 365 * (hours 24)

authenticate :: Config -> Integer -> ByteString -> Handler PersonId
authenticate config exp sessionId = do
  ePersonId <- redisWithConfig config $
               Redis.lookupSession exp sessionId
  case ePersonId of
    Left e -> throwError $ err401 { errBody = e }
    Right personId -> pure personId

authHandler :: Config -> AuthHandler Request PersonId
authHandler config = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = either throw401 (authenticate config expiration)
      (maybeToEither "Session cookie is missing" (getSessionId req))

newAuthCookie :: ByteString -> Integer -> SetCookie
newAuthCookie sessionId ttl =
  defaultSetCookie
  { setCookieName = "session"
  , setCookieValue = sessionId
  , setCookiePath = Just "/"
  , setCookieMaxAge = Just (secondsToDiffTime ttl)
  , setCookieHttpOnly = True
  -- , setCookieSecure = True
  }

genSessionId :: IO ByteString
genSessionId = fmap ("session:" <>) genRandom64

genRandom16 :: IO ByteString
genRandom16 = Base16.encode <$> getEntropy 16

genRandom64 :: IO ByteString
genRandom64 = Base64.encode <$> getEntropy 64

getSessionId :: Request -> Maybe ByteString
getSessionId req =
  maybe Nothing (lookup "session")
  $ parseCookies <$> lookup "Cookie" (requestHeaders req)


createSession :: PersonId -> App SetCookie
createSession personId = do
  sessionId <- liftIO $ genSessionId
  redis $ Redis.createSession sessionId personId expiration
  pure $ newAuthCookie sessionId oneYear


deleteSession :: MonadIO m => Config -> ByteString -> m SetCookie
deleteSession config sessionId = do
  redisWithConfig config $ Redis.deleteSession sessionId
  pure $ newAuthCookie "" 1


createInvite :: GameId -> App InviteCode
createInvite gameId = do
  rawInviteId <- liftIO genRandom16
  let inviteId = "invite:" <> rawInviteId
  redis $ Redis.createInvite gameId inviteId (hours 2)
  pure (InviteCode (T.decodeUtf8 rawInviteId))
