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

module Main where

import Network.Wai.Handler.Warp (run)

import API (app)
import Config

main :: IO ()
main = do
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 3000
  pgConnPool <- makePGPool env
  redisConn <- connectRedis
  proxyManager <- newProxyManager
  
  let config = Config
        { getPGConnPool = pgConnPool
        , getRedisConn = redisConn
        , getProxyManager = proxyManager
        , getEnv = env
        }

  run port
    . setLogger env
    $ app config
