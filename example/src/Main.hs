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
import Reflex.WebSocketServer
import Servant

type MyServer = "socket" :> Raw :<|> Raw

main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  -- Build the websocket handler and get back the threads that need killing
  ((triggerThread, serverApp), _, eventLoopThread) <- runReflexBase' app
  let socketServer = websocketsOr defaultConnectionOptions serverApp $ \_ respond ->
        respond $ responseLBS status400 [] "Not a WebSocket request"
      mainServer = socketServer :<|> serveDirectory "./static"
  -- Serve the websocket handler and static assets.
  -- Make sure we kill those threads (this is important in GHCi)
  -- TODO: Figure out how to use ResourceT to kill the threads?
  finally (run 8080 $ serve @MyServer Proxy mainServer) $ do
    killThread triggerThread
    killThread eventLoopThread

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
  -- Create an event that ticks once a second
  (tick, trigger) <- newTriggerEvent
  triggerThread <- liftIO . forkIO . forever $ trigger () >> threadDelay 1000000
  -- Create a websocket handler
  sockets $ \incomingMessages -> do
    -- Keep all connections in a map.
    -- Set values to the last received message.
    -- Delete connections that disconnect.
    messageState <- foldDyn (uncurry takeMessage) Map.empty incomingMessages
    let outgoingMessages = tag (current messageState) tick
    -- Every second, send each connection the last message they sent us and print it
    performEvent_ $ fmap (liftIO . print) outgoingMessages
    return (triggerThread, outgoingMessages)
  where
    takeMessage connId (ControlMessage (Close _ _)) = Map.delete connId
    takeMessage connId message = Map.insert connId message
