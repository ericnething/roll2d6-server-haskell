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

module Config
  ( Config(..)
  , Environment(..)
  , makePGPool
  , connectRedis
  , newProxyManager
  , setLogger
  , lookupSetting
  )
where


import Control.Exception (throwIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid ((<>))

import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.HTTP.Client
  ( Manager
  , defaultManagerSettings
  , newManager
  )

import Data.Pool (Pool, createPool)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis as Redis

import System.Environment (lookupEnv)
import Safe (readMay)


data Config = Config
  { getPGConnPool   :: Pool PG.Connection
  , getRedisConn    :: Redis.Connection
  , getProxyManager :: Manager
  , getEnv          :: Environment
  }

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | Choose a logger based on Environment
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout


makePGPool :: Environment -> IO (Pool PG.Connection)
makePGPool env =
  envPGConnString env >>= \connString ->
  createPool
  (PG.connectPostgreSQL connString)
  PG.close
  (envStripes env)
  -- (envTimeout env)
  60
  (envPool env)


-- | A basic 'ConnectionString' for local/test development. Pass in
-- either @""@ for 'Development' or @"test"@ for 'Test'.
envPGConnString :: Environment -> IO ByteString

envPGConnString Test = pure "host=localhost dbname=roll2d6-test user=roll2d6 password=roll2d6 port=5432"

envPGConnString Development = pure "host=localhost dbname=roll2d6 user=roll2d6 password=roll2d6 port=5432"

envPGConnString Production = do
    connString <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        pure $
          BS.intercalate " " $ zipWith (<>) keys (BS8.pack <$> envVars)

    case connString of
         Nothing ->
           throwIO (userError "Database Configuration not present in environment.")
         Just a ->
           pure a

-- | Number of pools to use for a given environment
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 10

-- | Number of stripes to use for a given environment
envStripes :: Environment -> Int
envStripes Test        = 2
envStripes Development = 2
envStripes Production  = 2

-- | Idle timeout for PG connections in a given environment
envTimeout :: Environment -> Int
envTimeout Test        = 60
envTimeout Development = 60
envTimeout Production  = 60

connectRedis :: IO Redis.Connection
connectRedis = Redis.checkedConnect Redis.defaultConnectInfo

newProxyManager :: IO Manager
newProxyManager = newManager defaultManagerSettings

-- | Look up an environment variable
lookupSetting :: Read a => String -> a -> IO a
lookupSetting var def = do
  menv <- lookupEnv var
  case menv of
    Nothing -> return def
    Just s  -> maybe (failed s) pure (readMay s)
  where
    failed s = error $
               "Failed to read \"" ++ s ++
               "\" for environment variable \"" ++ var ++ "\""
