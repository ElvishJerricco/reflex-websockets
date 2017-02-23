{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Reflex
import Reflex.Base
import Reflex.Spider.Internal
       (HasSpiderTimeline, SpiderTimelineEnv)
import Reflex.WebSocketServer
import Servant

type MyServer = "socket" :> Raw :<|> Raw

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  (serverApp, triggerThread, eventLoopThread) <-
    withSpiderTimeline mainWithTimeline
  let socketServer = defaultWebSockets serverApp
      staticServer = serveDirectory "./static"
      mainServer =
        run 8080 . serve @MyServer Proxy $ socketServer :<|> staticServer
      killThreads = traverse killThread [triggerThread, eventLoopThread]
  finally mainServer killThreads

defaultWebSockets :: ServerApp -> Application
defaultWebSockets serverApp =
  websocketsOr defaultConnectionOptions serverApp $ \_ respond ->
    respond $ responseLBS status400 [] "Not a WebSocket request"

mainWithTimeline
  :: HasSpiderTimeline x
  => SpiderTimelineEnv x -> IO (ServerApp, ThreadId, ThreadId)
mainWithTimeline t = do
  ((triggerThread, serverApp), _, eventLoopThread) <-
    runReflexBaseForTimeline' app t
  return (serverApp, triggerThread, eventLoopThread)

app
  :: ( MonadFix m
     , MonadHold t m
     , MonadIO m
     , PerformEvent t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     )
  => m (ThreadId, ServerApp)
app = do
  (tick, trigger) <- newTriggerEvent
  triggerThread <- liftIO . forkIO . forever $ trigger () >> threadDelay 1000000
  -- ^ Create an event that ticks once a second
  sockets $ \e -> do
    messageState <- foldDyn (uncurry takeMessage) Map.empty e
    -- ^ Keep all connections in a map
    -- Every second, send each connection the last message they sent us
    let messagesToSend = tag (current messageState) tick
    performEvent_ $ fmap (liftIO . print) messagesToSend
    return (triggerThread, messagesToSend)
  where
    takeMessage connId (ControlMessage (Close _ _)) = Map.delete connId
    takeMessage connId message = Map.insert connId message
