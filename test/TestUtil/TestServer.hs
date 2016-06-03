{-# LANGUAGE OverloadedStrings #-}

module TestUtil.TestServer
    ( Port
    , RequestHandlerResult(..)
    , startTestServer
    , stopTestServer
    , withTestServer
    ) where

import           Control.Concurrent
                    ( ThreadId
                    , forkIO
                    , killThread
                    )
import           Control.Concurrent.STM
                    ( STM
                    , TVar
                    , atomically
                    , modifyTVar'
                    , newTVarIO
                    )
import           Control.Exception (bracket)
import           Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Types
                    ( hContentType
                    , ok200
                    , serviceUnavailable503
                    )
import           Network.Wai
                    ( Application
                    , Request
                    , responseLBS
                    )
import           Network.Wai.Handler.Warp
                    ( Port
                    , defaultSettings
                    , runSettings
                    , setBeforeMainLoop
                    , setOnClose
                    , setOnOpen
                    , setPort
                    )

import Prelude
import TestUtil.Event

data RequestHandlerResult =
    Valid L.ByteString |
    Done L.ByteString |
    BadState |
    BadRequest
    deriving Show

type RequestHandler = Request -> STM RequestHandlerResult

type ServerInfo = (ThreadId, TMEvent, TVar Int)

-- | Start test server
-- Starts test server using specified request handler and returns
-- server information for use by stopTestServer etc.
startTestServer :: Port -> RequestHandler -> IO ServerInfo
startTestServer p h = do
    -- Signalled when server is ready to handle connections
    ready <- newMEvent

    -- Number of active connections
    connectionCount <- newTVarIO (0 :: Int)

    -- Signalled when server has shut down
    shutdown <- newTMEventIO

    -- Application settings including event handlers
    let settings =
            setPort p $
            setBeforeMainLoop (setMEvent ready) $
            setOnOpen (const $ atomically $ modifyTVar' connectionCount (+1) >> return True) $
            setOnClose (const $ atomically $ modifyTVar' connectionCount (subtract 1)) $
            defaultSettings

    -- Application handler
    let app :: Application
        app req respond = do
            response <- atomically $ do
                let textResponse = return . responseLBS ok200 [(hContentType, "text/plain")]
                shouldRun <- isSetTMEvent shutdown
                if shouldRun
                then do
                    (shouldShutDown, responseText) <- callback
                    when shouldShutDown $ setTMEvent shutdown
                    textResponse responseText
                else return $ responseLBS serviceUnavailable503 [] ""
            respond response
            where
                callback :: STM (Bool, L.ByteString)
                callback = do
                    r <- h req
                    return $ case r of
                        Valid s -> (False, s)
                        Done s -> (True, s)
                        BadState -> (True, "BadState error")
                        BadRequest -> (True, "BadRequest error")

    -- Run server on separate thread and wait until it's ready
    threadId <- forkIO $ runSettings settings app
    waitMEvent ready
    return (threadId, shutdown, connectionCount)

-- | Stop test server
-- Stops test server started using startTestServer function
stopTestServer :: ServerInfo -> IO ()
stopTestServer (threadId, _, _) = killThread threadId
{-
stopTestServer (threadId, shutdown, connectionCount) = atomically $ do
    waitTMEvent shutdown
    value <- readTVar connectionCount
    when (value /= 0)
        retry
-}

-- | Run test server
-- Wraps execution of test server using given request handler
withTestServer :: Port -> RequestHandler -> IO () -> IO ()
withTestServer p h action = bracket (startTestServer p h) stopTestServer (const action)
