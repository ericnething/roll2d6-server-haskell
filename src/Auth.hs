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

import Data.Monoid (mconcat, (<>))
import Control.Monad (when, join)
import Control.Monad.IO.Class (liftIO)

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types
  ( unauthorized401
  , Status
  , status200
  , status400
  , status500
  )
import Web.Cookie (parseCookies)
import Servant
import Servant.Server.Experimental.Auth

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Builder (word8Hex, toLazyByteString)
import           System.Entropy (getEntropy)

import Database (verifyGameAccess)
import Types
  ( App
  , PersonId(..)
  , GameId(..)
  , AccessLevel
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
        

setAuthCookie :: ByteString -> Integer -> App ()
setAuthCookie sessionId exp = do pure ()
  -- setHeader "Set-Cookie" (newAuthCookie sessionId exp)

newAuthCookie :: ByteString -> Integer -> LT.Text
newAuthCookie sessionId ttl =
  LT.intercalate "; "
  [ "session=" <> (LT.fromStrict . T.decodeUtf8 $ sessionId)
  , "HttpOnly"
    -- , "Secure"
  , "Max-Age=" <> (LT.pack . show $ ttl)
  , "Path=/"
  ]

genSessionId :: IO ByteString
genSessionId = fmap ("session:" <>) genRandomId

genRandomId :: IO ByteString
genRandomId = do
  randBytes <- getEntropy 32
  return $ prettyPrint randBytes
  where
    prettyPrint :: ByteString -> ByteString
    prettyPrint
      = LBS.toStrict
      . toLazyByteString
      . mconcat
      . BS.foldr'
      ( \ byte acc -> word8Hex byte:acc ) []


getSessionId :: Request -> Maybe ByteString
getSessionId req =
  maybe Nothing (lookup "session")
  $ parseCookies <$> lookup "Cookie" (requestHeaders req)


createSession :: PersonId -> App ()
createSession personId = do
  sessionId <- liftIO $ genSessionId
  redis $ Redis.createSession sessionId personId expiration
  setAuthCookie sessionId oneYear


deleteSession :: ByteString -> App ()
deleteSession sessionId = do
  redis $ Redis.deleteSession sessionId
  setAuthCookie "" 1


createInvite :: GameId -> App InviteCode
createInvite gameId = do
  rawInviteId <- liftIO genRandomId
  let inviteId = "invite:" <> rawInviteId
  redis $ Redis.createInvite gameId inviteId (hours 2)
  pure (InviteCode (T.decodeUtf8 rawInviteId))
