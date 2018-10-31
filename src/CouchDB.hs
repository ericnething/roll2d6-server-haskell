{-# LANGUAGE OverloadedStrings #-}

module CouchDB
  ( createDatabase )
where

import Data.Aeson
import Data.Default.Class
import Network.HTTP.Req
import Data.Text (Text)

couchDomain = "localhost"
couchPort = 5984

createDatabase :: Text -> IO (Maybe ())
createDatabase dbname =
  runReq def $ do
  -- let body = object [ "db" .= dbname ]
  r <- req PUT
    (http couchDomain /: dbname)
    NoReqBody
    -- (ReqBodyJson body)
    ignoreResponse
    (port couchPort)
  pure $
    case responseStatusCode r of
      201 -> Just ()
      _   -> Nothing
