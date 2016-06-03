module TestUtil.Event
    ( MEvent
    , TMEvent
    , isSetMEvent
    , isSetTMEvent
    , newMEvent
    , newTMEventIO
    , setMEvent
    , setTMEvent
    , waitMEvent
    , waitTMEvent
    ) where

import Control.Concurrent
            ( MVar
            , isEmptyMVar
            , newEmptyMVar
            , putMVar
            , takeMVar
            )
import Control.Concurrent.STM
            ( STM
            )
import Control.Concurrent.STM.TMVar
            ( TMVar
            , isEmptyTMVar
            , newEmptyTMVarIO
            , putTMVar
            , takeTMVar
            )

import Prelude

type MEvent = MVar ()

isSetMEvent :: MEvent -> IO Bool
isSetMEvent = isEmptyMVar

newMEvent :: IO MEvent
newMEvent = newEmptyMVar

setMEvent :: MEvent -> IO ()
setMEvent = (flip putMVar) ()

waitMEvent :: MEvent -> IO ()
waitMEvent = takeMVar

type TMEvent = TMVar ()

isSetTMEvent :: TMEvent -> STM Bool
isSetTMEvent = isEmptyTMVar

newTMEventIO :: IO TMEvent
newTMEventIO = newEmptyTMVarIO

setTMEvent :: TMEvent -> STM ()
setTMEvent = (flip putTMVar) ()

waitTMEvent :: TMEvent -> STM ()
waitTMEvent = takeTMVar
