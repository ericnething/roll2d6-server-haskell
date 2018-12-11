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
