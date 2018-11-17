{-# LANGUAGE OverloadedStrings #-}

{-|
    A WAI adapter to the HTML5 Server-Sent Events API.
-}
module EventSource
  ( ServerEvent(..)
  , eventSourceAppTChan
  , eventSourceAppIO
  )
where

import           Data.Function (fix)
-- import           Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan
  ( TChan
  , dupTChan
  , readTChan
  )
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types (status200, hContentType)
import           Network.Wai (Application, responseStream)
import Data.ByteString (ByteString)

import Network.Wai.EventSource.EventStream

-- | Make a new WAI EventSource application reading events from
-- the given channel.
eventSourceAppTChan :: ByteString
                    -> TChan (ByteString, ServerEvent)
                    -> Application
eventSourceAppTChan key chan req sendResponse = do
    chan' <- liftIO . atomically $ dupTChan chan
    eventSourceAppIO key (atomically $ readTChan chan') req sendResponse

-- | Make a new WAI EventSource application reading events from
-- the given IO action.
eventSourceAppIO :: ByteString
                 -> IO (ByteString, ServerEvent)
                 -> Application
eventSourceAppIO key src _ sendResponse =
    sendResponse $ responseStream
        status200
        [(hContentType, "text/event-stream")]
        $ \sendChunk flush -> fix $ \loop -> do
            (key_, se) <- src
            let event =
                  if key == key_ || "*" == key_
                  then eventToBuilder se
                  else Nothing
            case event of
              Nothing -> return ()
              Just b  -> sendChunk b >> flush >> loop
