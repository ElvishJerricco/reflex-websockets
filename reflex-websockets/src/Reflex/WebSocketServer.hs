{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Reflex.WebSocketServer where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens.Indexed
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import Network.WebSockets
import Reflex

data ConnectionId =
  ConnectionId ThreadId
  deriving (Eq, Ord, Show)

sockets
  :: (MonadIO m, PerformEvent t m, MonadIO (Performable m), TriggerEvent t m)
  => (Event t (ConnectionId, Message) -> m (a, Event t (Map ConnectionId Message)))
  -> m (a, ServerApp)
sockets f = do
  (incoming, pushIncoming) <- newTriggerEvent
  wOutgoing <- liftIO newChan
  (result, outgoing) <- f incoming
  performEvent_ . ffor outgoing . itraverse_ $ \connId outgoingMessage ->
    liftIO $ writeChan wOutgoing (connId, outgoingMessage)
  return . (,) result $ \pc ->
    handle @SomeException print $ do
      rOutgoing <- dupChan wOutgoing
      conn <- acceptRequest pc
      connId <- myThreadId
      let outgoingLoop =
            forever $ do
              (ConnectionId outgoingId, outgoingMessage) <- readChan rOutgoing
              when (connId == outgoingId) $ send conn outgoingMessage
          incomingLoop =
            forever $ do
              incomingMessage <- receive conn
              pushIncoming (ConnectionId connId, incomingMessage)
      void $ concurrently outgoingLoop incomingLoop

sockets_
  :: (MonadIO m, PerformEvent t m, MonadIO (Performable m), TriggerEvent t m)
  => (Event t (ConnectionId, Message) -> m (Event t (Map ConnectionId Message)))
  -> m ServerApp
sockets_ f = snd <$> sockets (fmap ((,) ()) . f)
