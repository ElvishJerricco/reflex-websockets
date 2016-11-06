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
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Reflex
import Reflex.Base
import Reflex.WebSocketServer
import Servant

type MyServer = "socket" :> Raw :<|> Raw

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  (serverApp, triggerThread, eventLoopThread, printThread) <-
    withSpiderTimeline mainWithTimeline
  let socketServer = defaultWebSockets serverApp
      staticServer = serveDirectory "./static"
      mainServer =
        run 8080 . serve @MyServer Proxy $ socketServer :<|> staticServer
      killThreads =
        traverse killThread [triggerThread, eventLoopThread, printThread]
  finally mainServer killThreads

defaultWebSockets :: ServerApp -> Application
defaultWebSockets serverApp =
  websocketsOr defaultConnectionOptions serverApp $ \_ respond ->
    respond $ responseLBS status400 [] "Not a WebSocket request"

mainWithTimeline :: SpiderTimeline x
                 -> IO (ServerApp, ThreadId, ThreadId, ThreadId)
mainWithTimeline t = do
  (((messageState, triggerThread), serverApp), _, eventLoopThread) <-
    runReflexBase' app t
  -- Print the current state every five seconds
  printThread <-
    forkIO . forever $ do
      print =<< runSpiderHostForTimeline (sample (current messageState)) t
      threadDelay 5000000
  return (serverApp, triggerThread, eventLoopThread, printThread)

app
  :: ( MonadFix m
     , MonadHold t m
     , MonadIO m
     , PerformEvent t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     )
  => m ((Dynamic t (Map ConnectionId Message), ThreadId), ServerApp)
app = do
  (tick, trigger) <- newTriggerEvent
  -- ^ Create an event that ticks once a second
  triggerThread <- liftIO . forkIO . forever $ trigger () >> threadDelay 1000000
  sockets $ \e -> do
    messageState <- foldDyn (uncurry takeMessage) Map.empty e
    -- ^ Keep all connections in a map
    -- Every second, send each connection the last message they sent us
    return . (,) (messageState, triggerThread) $
      tagPromptlyDyn messageState tick
  where
    takeMessage connId (ControlMessage (Close _ _)) = Map.delete connId
    takeMessage connId message = Map.insert connId message
