{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.Ref
import Data.Dependent.Sum
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Traversable

import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.Spider.Internal (globalSpiderTimelineEnv)
import Reflex.TriggerEvent.Base

--------------------------------------------------------------------------------

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
import Reflex.Spider.Internal
       (HasSpiderTimeline, SpiderTimelineEnv)
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

mainWithTimeline
  :: HasSpiderTimeline x
  => SpiderTimelineEnv x -> IO (ServerApp, ThreadId, ThreadId, ThreadId)
mainWithTimeline t = do
  (((messageState, triggerThread), serverApp), _, eventLoopThread) <-
    runReflexBaseForTimeline' app t
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

--------------------------------------------------------------------------------

type ReflexBase x = PostBuildT (SpiderTimeline x) (TriggerEventT (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)))

-- | Runs a program in a given timeline. Returns the result of the
-- program, a firing command for firing events in the program, and the
-- thread id for the event firing thread.
runReflexBaseForTimeline'
  :: HasSpiderTimeline x
  => ReflexBase x a
  -> SpiderTimelineEnv x
  -> IO (a, FireCommand (SpiderTimeline x) (SpiderHost x), ThreadId)
runReflexBaseForTimeline' basic t = do
  events <- newChan
  (result, FireCommand fire) <-
    flip runSpiderHostForTimeline t $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      -- Run the various layers of event logic that exist over 'SpiderHost'
      results@(_, FireCommand fire) <-
        hostPerformEventT
          (runTriggerEventT (runPostBuildT basic postBuild) events)
      -- Read the trigger's ref, traverse into its 'Maybe', and fire the event
      readRef postBuildTriggerRef >>=
        traverse_
          (\postBuildTrigger ->
             fire [postBuildTrigger :=> Identity ()] $ return ())
      return results
  -- Start a thread that fires events forever
  eventThreadId <-
    forkIO . forever $ do
      triggers <- readChan events
      void . flip runSpiderHostForTimeline t $ do
        triggersToFire <-
          liftIO . for triggers $ \(EventTriggerRef tref :=> TriggerInvocation a _) ->
            fmap (\trigger -> trigger :=> Identity a) <$> readRef tref
        void . fire (catMaybes triggersToFire) $ return ()
        liftIO . for_ triggers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return ()
  return (result, FireCommand fire, eventThreadId)

-- | Like 'runReflexBasic'', but discards the less relevant outputs.
runReflexBaseForTimeline
  :: HasSpiderTimeline x
  => ReflexBase x a -> SpiderTimelineEnv x -> IO a
runReflexBaseForTimeline basic =
  fmap (\(a, _, _) -> a) . runReflexBaseForTimeline' basic

runReflexBase'
  :: ReflexBase Global a
  -> IO (a, FireCommand (SpiderTimeline Global) (SpiderHost Global), ThreadId)
runReflexBase' basic = runReflexBaseForTimeline' basic globalSpiderTimelineEnv

runReflexBase :: ReflexBase Global a -> IO a
runReflexBase basic = runReflexBaseForTimeline basic globalSpiderTimelineEnv
