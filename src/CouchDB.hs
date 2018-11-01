{-# LANGUAGE OverloadedStrings #-}

module CouchDB
  ( createDatabase )
where

import Data.Aeson
import Data.Default.Class
import Network.HTTP.Req
import Types (GameId)
import qualified Data.Text as T (pack)

couchDomain = "localhost"
couchPort = 5984

createDatabase :: GameId -> IO (Maybe ())
createDatabase gameId =
  runReq def $ do
  r <- req PUT
    (http couchDomain /: T.pack (show gameId))
    NoReqBody
    ignoreResponse
    (port couchPort)
  pure $
    case responseStatusCode r of
      201 -> Just ()
      _   -> Nothing
